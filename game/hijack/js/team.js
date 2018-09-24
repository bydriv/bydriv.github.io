export function enemy(team1, team2) {
    switch (team1) {
    case "player":
        switch (team2) {
        case "player":
            return false;
        case "enemy":
            return true;
        }
    case "enemy":
        switch (team2) {
        case "player":
            return true;
        case "enemy":
            return false;
        }
    }
}

export function color(team) {
    switch (team) {
    case "player":
        return {
            fg: 0x00FF00,
            bg: 0x008000
        };
    case "enemy":
        return {
            fg: 0xFF0000,
            bg: 0x800000
        };
    }
}
