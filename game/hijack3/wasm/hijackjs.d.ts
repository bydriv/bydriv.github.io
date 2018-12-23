/* tslint:disable */
export function views_length(arg0: Views): number;

export function view_is_image(arg0: number, arg1: Views): boolean;

export function view_image_name(arg0: number, arg1: Views): string;

export function view_image_x(arg0: number, arg1: Views): number | undefined;

export function view_image_y(arg0: number, arg1: Views): number | undefined;

export function new_(): Game;

export function step(arg0: any, arg1: Game): Game;

export function views(arg0: Game): Views;

export class Views {
free(): void;

}
export class Game {
free(): void;

}
