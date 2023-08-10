var config = {
    type: Phaser.AUTO,
    width: window.innerWidth,
    height: window.innerHeight,
    scene: {
        create: create,
        update: update
    }
};

var player1, player2;
var cursors1, cursors2;
var bullets1, bullets2;
var shootKey1, shootKey2;
var score1 = 0;  // 添加这一行
var score2 = 0;  // 添加这一行

var game = new Phaser.Game(config);

function create() {
    this.myScene = this;

    var gameWidth = this.sys.game.config.width;
    var gameHeight = this.sys.game.config.height;

    // 例如，如果您想让 player1 在游戏宽度的 10% 的位置，高度的 50% 的位置
    player1 = this.add.rectangle(gameWidth * 0.1, gameHeight * 0.5, 50, 50, 0xff0000).setOrigin(0);

    // player2 在游戏宽度的 90% 的位置，高度的 50% 的位置
    player2 = this.add.rectangle(gameWidth * 0.9, gameHeight * 0.5, 50, 50, 0x0000ff).setOrigin(0);

    cursors1 = this.input.keyboard.addKeys({ up: 'W', down: 'S', left: 'A', right: 'D' });
    cursors2 = this.input.keyboard.addKeys({ up: 'UP', down: 'DOWN', left: 'LEFT', right: 'RIGHT' });

    bullets1 = this.add.group();
    bullets2 = this.add.group();

    shootKey1 = this.input.keyboard.addKey(Phaser.Input.Keyboard.KeyCodes.SPACE);
    shootKey2 = this.input.keyboard.addKey(Phaser.Input.Keyboard.KeyCodes.ENTER);

    // 添加计分文本
    scoreText1 = this.add.text(16, 16, 'Player 1: 0', { fontSize: '32px', fill: '#fff' });
    scoreText2 = this.add.text(16, 48, 'Player 2: 0', { fontSize: '32px', fill: '#fff' });
}

function update() {
    movePlayer(player1, cursors1, 5);
    movePlayer(player2, cursors2, 5);

    if (shootKey1.isDown) {
        shoot.call(this, player1, bullets1, 10, 0xff0000, cursors1);
    }
    
    if (shootKey2.isDown) {
        shoot.call(this, player2, bullets2, 10, 0x0000ff, cursors2);
    }
    

    bullets1.getChildren().forEach(bullet => {
        bullet.x += bullet.speedX;
        bullet.y += bullet.speedY;
        if (bullet.x > this.sys.canvas.width || bullet.x < 0 || bullet.y > this.sys.canvas.height || bullet.y < 0) {
            bullet.destroy();
        }
    });
    
    bullets2.getChildren().forEach(bullet => {
        bullet.x += bullet.speedX;
        bullet.y += bullet.speedY;
        if (bullet.x > this.sys.canvas.width || bullet.x < 0 || bullet.y > this.sys.canvas.height || bullet.y < 0) {
            bullet.destroy();
        }
    });
    

    checkCollision(player2, bullets1, hitPlayer2);
    checkCollision(player1, bullets2, hitPlayer1);
}

function checkCollision(player, bullets, callback) {
    bullets.getChildren().forEach(bullet => {
        if (Phaser.Geom.Intersects.RectangleToRectangle(player.getBounds(), bullet.getBounds())) {
            callback(player, bullet);
        }
    });
}

function movePlayer(player, cursors, speed) {
    if (cursors.left.isDown) {
        player.x -= speed;
    } else if (cursors.right.isDown) {
        player.x += speed;
    }

    if (cursors.up.isDown) {
        player.y -= speed;
    } else if (cursors.down.isDown) {
        player.y += speed;
    }
}


function moveBullet(bullet, speed) {
    bullet.x += speed;
    if (bullet.x > 800) bullet.destroy(); // 销毁离开屏幕的子弹
}


function shoot(player, bullets, speed, color, cursors) {
    var bullet = this.add.rectangle(player.x + player.width, player.y + player.height / 2, 10, 5, color).setOrigin(0);
    bullets.add(bullet);

    if (cursors.left.isDown) {
        bullet.speedX = -speed;
        bullet.speedY = 0;
    } else if (cursors.right.isDown) {
        bullet.speedX = speed;
        bullet.speedY = 0;
    } else if (cursors.up.isDown) {
        bullet.speedX = 0;
        bullet.speedY = -speed;
    } else if (cursors.down.isDown) {
        bullet.speedX = 0;
        bullet.speedY = speed;
    } else { // 如果没有按下方向键，子弹默认向前飞行
        bullet.speedX = player === player1 ? speed : -speed;
        bullet.speedY = 0;
    }
}




function hitPlayer1(player, bullet) {
    bullet.destroy();
    score2 += 10;
    scoreText2.setText('Player 2: ' + score2);
}

function hitPlayer2(player, bullet) {
    bullet.destroy();
    score1 += 10;
    scoreText1.setText('Player 1: ' + score1);
}