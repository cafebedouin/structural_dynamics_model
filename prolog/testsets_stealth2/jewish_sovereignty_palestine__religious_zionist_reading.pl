% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__religious_zionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__religious_zionist_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: jewish_sovereignty_palestine__religious_zionist_reading
 *   human_readable: Divine-Title Territorial Claim — Religious Zionist Reading of Jewish Sovereignty in Palestine
 *   domain: political philosophy / nationalism studies / postcolonial theory
 *
 * SUMMARY:
 *   This story instantiates the religious Zionist reading of the
 *   Jewish-sovereignty kernel: the divine promise of Eretz Yisrael grounds an
 *   inalienable territorial title, and sovereign statehood is not merely
 *   permitted but is the vehicle of redemption. Because the title is divine,
 *   it admits no counterparty: no Palestinian consent, no negotiated border,
 *   and no partition can extinguish it, and populations living under the
 *   title-holding sovereign are governed by instruments (military law, land
 *   declarations, permit regimes) that require no assent from them. The
 *   claim/metric gap is deliberate and load-bearing: the reading itself
 *   narrates the arrangement as fulfillment and gift, while the authored
 *   metrics describe a structure in which one community collects identity,
 *   land, and legitimacy and another community bears displacement,
 *   statelessness, and subordination with no offsetting benefit and no exit.
 *   The engine measures that divergence; this file does not reconcile it. KEY
 *   AGENTS (by structural relationship): - religious_zionist_leadership:
 *   Primary agenda-setter (organized/identity_locked) — articulates and
 *   certifies the covenantal claim - israeli_state_enforcement_apparatus:
 *   Co-agenda-setter and beneficiary (institutional/constrained) —
 *   administers and garrisons the arrangement -
 *   messianic_settlement_vanguard: Concentrated beneficiary
 *   (organized/identity_locked) — enacts the claim physically on the land -
 *   jewish_covenant_community: Diffuse beneficiary (powerful/mobile) —
 *   collects identity-cohesion and legitimating narrative -
 *   west_bank_palestinian_residents: Primary target (powerless/trapped) —
 *   governed by military law, land allocated away -
 *   palestinian_refugee_diaspora: Intergenerational target
 *   (powerless/trapped) — return foreclosed by the claim's demography -
 *   palestinian_citizens_of_israel: Subordinate target (moderate/constrained)
 *   — formal equality inside a communal hierarchy -
 *   international_legal_order: Excluded objector (institutional/analytical) —
 *   pronounces unlawfulness, is dismissed as jurisdiction-less -
 *   secular_israeli_peace_camp: Excluded internal objector
 *   (moderate/constrained) — partition preference stripped of standing inside
 *   the framework
 *
 * KEY AGENTS:
 *   - religious_zionist_leadership: agenda-setting doctrinal authority (organized power, identity_locked exit, national scope)
 *   - israeli_state_enforcement_apparatus: administering and enforcing seat with a mixed ledger (institutional power, constrained exit, national scope)
 *   - messianic_settlement_vanguard: concentrated on-the-ground beneficiary and accelerant (organized power, identity_locked exit, regional scope)
 *   - jewish_covenant_community: diffuse beneficiary of identity and legitimacy (powerful, mobile exit, global scope)
 *   - west_bank_palestinian_residents: primary bearing seat under military law (powerless, trapped, regional scope)
 *   - palestinian_refugee_diaspora: intergenerational bearing seat denied return (powerless, trapped, continental scope)
 *   - palestinian_citizens_of_israel: subordinate citizen seat (moderate, constrained, national scope)
 *   - international_legal_order: excluded external objector (institutional, analytical, global scope)
 *   - secular_israeli_peace_camp: excluded internal objector (moderate, constrained, national scope)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__religious_zionist_reading, 0.88).
domain_priors:suppression_score(jewish_sovereignty_palestine__religious_zionist_reading, 0.82).
domain_priors:theater_ratio(jewish_sovereignty_palestine__religious_zionist_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__religious_zionist_reading, snare).
narrative_ontology:human_readable(jewish_sovereignty_palestine__religious_zionist_reading, "Divine-Title Territorial Claim — Religious Zionist Reading of Jewish Sovereignty in Palestine").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__religious_zionist_reading, "political philosophy / nationalism studies / postcolonial theory").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__religious_zionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__religious_zionist_reading, '4da0b410-489b-4173-8c62-cda4dd98632b').
narrative_ontology:cs_kernel_codification('4da0b410-489b-4173-8c62-cda4dd98632b', fixed_text).
narrative_ontology:cs_authority_grounding('4da0b410-489b-4173-8c62-cda4dd98632b', lineage).
narrative_ontology:cs_interpretation_layer_present('4da0b410-489b-4173-8c62-cda4dd98632b').
narrative_ontology:cs_reading_relation('4da0b410-489b-4173-8c62-cda4dd98632b', jewish_sovereignty_palestine__cultural_zionist_reading, forecloses).
narrative_ontology:cs_reading_relation('4da0b410-489b-4173-8c62-cda4dd98632b', jewish_sovereignty_palestine__liberal_nationalist_reading, influences).
narrative_ontology:cs_reading_relation('4da0b410-489b-4173-8c62-cda4dd98632b', jewish_sovereignty_palestine__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('4da0b410-489b-4173-8c62-cda4dd98632b', jewish_sovereignty_palestine__post_zionist_reading, forecloses).
narrative_ontology:cs_axiom('4da0b410-489b-4173-8c62-cda4dd98632b', foundational, eretz_yisrael_divine_title_inalienable).
narrative_ontology:cs_axiom_status(eretz_yisrael_divine_title_inalienable, holdable).
narrative_ontology:cs_axiom_grounding('4da0b410-489b-4173-8c62-cda4dd98632b', eretz_yisrael_divine_title_inalienable, theological).
narrative_ontology:cs_axiom('4da0b410-489b-4173-8c62-cda4dd98632b', foundational, statehood_messianic_fulfillment).
narrative_ontology:cs_axiom_status(statehood_messianic_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('4da0b410-489b-4173-8c62-cda4dd98632b', statehood_messianic_fulfillment, theological).
narrative_ontology:cs_reference_frame('4da0b410-489b-4173-8c62-cda4dd98632b', divine_covenant_land_grant).
narrative_ontology:cs_drift_state('4da0b410-489b-4173-8c62-cda4dd98632b', contemporary_annexationist_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4da0b410-489b-4173-8c62-cda4dd98632b', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__religious_zionist_reading, jewish_covenant_community).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__religious_zionist_reading, messianic_settlement_vanguard).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__religious_zionist_reading, israeli_state_enforcement_apparatus).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__religious_zionist_reading, west_bank_palestinian_residents).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_refugee_diaspora).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_citizens_of_israel).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rabbis, yeshiva heads, and movement institutions articulate the covenantal claim to the whole land, train and certify the settlement enterprise, and supply the doctrinal justification that state policy draws on. Their standing depends on the claim remaining open; renouncing territory would dissolve the movement's reason for being. Leaving would mean abandoning the lifework of their communities and students.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, religious_zionist_leadership, agenda_setter,
    organized, generational, identity_locked, national).

% Administers the territories through military orders, permits, state-land declarations, and the court system; extends roads, utilities, and army protection to settlements; drafts its own citizens to garrison the arrangement. Gains territorial depth and a legitimating national story; pays in conscription burden, treasury outlays, and standing with allies. Could in principle negotiate the arrangement away, but coalition arithmetic and settler constituencies make that path costly.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, israeli_state_enforcement_apparatus, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__religious_zionist_reading, israeli_state_enforcement_apparatus, beneficiary).

% Hilltop pioneers, settlement mayors, and youth-movement graduates who physically enact the claim — building, planting, holding ground. Receive land allocations, subsidies, and army protection; fuse personal biography with the redemptive project so that leaving reads as betrayal of the whole enterprise. Continuously push the state further than it would otherwise go.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, messianic_settlement_vanguard, beneficiary,
    organized, civilizational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__religious_zionist_reading, messianic_settlement_vanguard, agenda_setter).

% World Jewry and Israeli Jewish society as the collective addressed by the promise. Receives identity-cohesion, an answer to the question of why sovereignty belongs here, and the ingathering narrative; most bear no daily cost, though a subset shoulders conscription and security exposure. Diaspora members can distance themselves from the claim at little price; Israeli members cannot.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, jewish_covenant_community, beneficiary,
    powerful, generational, mobile, global).

% Roughly three million people governed by military law rather than by citizenship in the sovereign that rules them. Land is declared state property and allocated to settlements; movement runs through checkpoints and permit queues; demolitions and expulsion orders arrive administratively. No vote selects the government that governs them, and the claim's terms leave no recognized path to statehood on any part of the land.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, west_bank_palestinian_residents, payer,
    powerless, biographical, trapped, regional).

% Millions of descendants of those displaced in 1948 and after, living in camps and exile communities across neighboring states. Return is the remedy international law holds out to them, and it is precisely what the claim's demographic logic forbids; their counterclaim is answered with resettlement-elsewhere proposals they reject. Their position cannot be exited individually without dissolving the community's own history.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_refugee_diaspora, payer,
    powerless, generational, trapped, continental).

% About a fifth of the state's citizens: voting, taxed, and represented, yet marked by the Nation-State Law as outside the self-definition the arrangement protects. Encounter land regimes, planning discrimination, and loyalty suspicion; emigration is available but means leaving home. Their formal equality sits inside a constitutional order that names the polity as belonging to another people.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_citizens_of_israel, payer,
    moderate, biographical, constrained, national).

% UN bodies, the International Court of Justice, and the treaty system pronounce the settlement enterprise unlawful and the occupation regime in breach of the Geneva Conventions. The reading dismisses these pronouncements as lacking jurisdiction over a divine grant. The pronouncements continue, altering nothing inside the framework while raising the external price of formal annexation.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, international_legal_order, excluded,
    institutional, generational, analytical, global).

% Israeli Jews who favor partition, power-sharing, or civic nationalism. Inside the covenant framework their position reads not as a rival policy but as faithlessness, which strips it of standing; they organize, march, and vote within a coalition system in which religious kingmakers price their preferences out.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, secular_israeli_peace_camp, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_sovereignty_palestine__religious_zionist_reading, messianic_settlement_vanguard).
narrative_ontology:fixing_cost_class(jewish_sovereignty_palestine__religious_zionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the Jewish national-religious community around a shared sacred geography and historical mission; answers the legitimacy question of why a dispersed people returns to this specific land; binds secular and religious Jews into one national project with transcendent warrant that no negotiated alternative supplies.
% TRANSFER_FUNCTION: Moves land, water, movement freedom, legal standing, and political voice from Palestinians — residents under military law, the refugee diaspora, and the state's own Palestinian citizens — to the Jewish covenant community and its settlement vanguard; moves conscription risk, treasury cost, and diplomatic exposure onto the enforcing state.
% ABSENT_VOICES: Three objector classes are outside the room the reading recognizes. Palestinians under the regime: their consent is structurally unnecessary, since a divine grant requires no counterparty assent. The international legal order: it pronounces continuously but is dismissed as jurisdiction-less over covenant. The secular Israeli peace camp: its partition preference is recast inside the framework as faithlessness rather than policy. None of the three can move the arrangement from where they stand; unanimity inside the framework is therefore an artifact of who was admitted, not of agreement.
% DISAPPEARANCE_RATIONALE: If the divine-title claim vanished overnight, the settlement enterprise loses its warrant, annexation agendas collapse, coalition politics reshuffle around the newly optional religious parties, and the state faces a suddenly negotiable border question; Palestinian legal status becomes contestable on ordinary terms. The physical arrangements — settlements, jurisdictions, permit grids — would not evaporate, but every one of them would lose the justification that currently makes them non-negotiable, and the region's diplomacy would reorganize around partition or federation as live options.
% FOUNDING_PROBLEM: The survival and continuity of a stateless, persecuted people, together with the theological scandal of a promise apparently unfulfilled: where does the covenant community persist, and what answers the exile? The arrangement was built to solve peoplehood-without-place by making the land itself the guarantee and return the redemption.
% FOUNDING_PROBLEM_CORROBORATION: The original problem is abundantly corroborated from outside the benefiting parties — the historical record of expulsions, pogroms, and twentieth-century statelessness is documented far beyond the movement's own testimony. Its CURRENT live status is not: no seat outside the beneficiary set attests that the problem remains unsolved. External historiography (including Israeli post-Zionist scholars) and the international legal order date the survival problem's resolution to 1948 and read the residual 'incomplete redemption' as a problem the claim generates for itself. The only attestations of a live founding problem come from within the movement — which is itself signal.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__religious_zionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__religious_zionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__religious_zionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_sovereignty_palestine__religious_zionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__religious_zionist_reading, 0.88, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__religious_zionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_sovereignty_palestine__religious_zionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_sovereignty_palestine__religious_zionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.88 because the asymmetry is total and by design: the title-claim requires no counterparty assent, so every unit of value the arrangement moves (land, water, movement freedom, legal standing, political voice) moves from seats that never agreed to the arrangement toward seats that need not compensate them, and the reading's own terms recognize no partition exit that would cap the transfer. Suppression is authored at 0.82 as a raw structural property — deliberately unscaled by power or scope, per the framework's separation — reflecting the enforcement machinery the arrangement requires: military orders, the checkpoint and permit grid, administrative demolition, settlement protection duty. Theater is 0.38 and rising slowly: the belief is sincere rather than performed, but a growing share of activity is curatorial — heritage site development, biblical archaeology, anniversary pageantry, juridical defense of 'facts' — as the practical frontier narrows. Accessibility collapse is 0.85: once the divine grant is accepted, partition and power-sharing collapse as thinkable options for holders, and for the bearing seats the permit regime has collapsed physical alternatives. Resistance is 0.7: two uprisings, boycott movements, advisory opinions, and a domestic peace camp — real, recurrent, and repeatedly absorbed into a higher baseline rather than decisive. The temporal series run on one shared seven-point grid (every tracked metric authored at every point, 1967–2025); the trajectory is a monotonic ratchet rather than a cycle — the intifadas spike enforcement and extraction, and each spike settles at a higher plateau than the preceding trough, which is why suppression_requirement is tracked here: the story's dynamic is the maturation and hardening of enforcement capacity, not merely shifting extraction.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute radically different types from identical structure. From the vanguard seat the arrangement is sacred obligation — coordination of self with covenant, extraction invisible by construction, exit unthinkable. From the covenant-community seat it is mostly gift: identity, meaning, and a solved legitimacy question, with costs (conscription, treasury, reputation) borne by the specialized enforcing subset. From the state seat the ledger is genuinely mixed — territorial depth and narrative against blood, budget, and isolation. From the three Palestinian seats the same structure is unmitigated bearing with no offsetting flow: not a fee paid for services rendered but a status imposed without consent. The engine computes these per-seat classifications from the structural data; the divergence between the vanguard's computed experience and the residents' computed experience is the perspectival measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: the covenant community (mobile exit — diaspora members can disengage at near-zero cost, placing them nearest the beneficiary end), the vanguard (identity_locked, but locked INTO benefit — the lock stabilizes its subsidized position), and the state apparatus (net beneficiary: it gains territory and narrative while paying conscription, budget, and reputational costs that keep it from the extreme beneficiary end). Victim declarations drive high directionality: West Bank residents (trapped, governed without the franchise — effectively full-target), the refugee diaspora (intergenerationally trapped — the one remedy international law offers, return, is precisely what the claim's demography forbids), and Palestinian citizens of Israel (listed among victims because their formal equality coexists with constitutional subordination and land-regime exclusion; the derivation correctly reads their victims-array membership rather than their nominal citizenship). No directionality overrides are authored: the beneficiary/victim declarations plus exit atoms produce the correct ordering without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — securing the survival and continuity of a stateless, persecuted people, and resolving the theological scandal of an unfulfilled promise — was substantially solved as a survival matter by 1948: sovereignty exists, refuge exists, ingathering happened. What persists is not the original function but a transformed mandate: completion of territorial redemption, a goal that recedes as it advances, since every withdrawal is readable as theft from the promise and every delay as exile prolonged. The R5 mismatch signature is present and expected: founding_problem_status is contested while disappearance_verdict is world_rearranges — the arrangement would leave an enormous rearranging vacuum overnight, yet the problem it was built to solve is disputed as still-live only by its own beneficiaries. That mismatch routes the capture/zombie flag for cross-checking against the computed piton/theater path. The mandatrophy discipline cuts both ways here: it forbids erasing the genuine coordination achievement (a scattered people was gathered and kept alive — real coordination, real benefit) and equally forbids letting that achievement launder the post-1967 accumulation, which serves a mandate the original problem no longer requires.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is the religious_zionist_reading of kernel jewish_sovereignty_palestine; which structural elements change under the four sibling readings?',
    'Diff the four sibling stories over the identical referent: liberal_nationalist admits partition legitimacy (victim set shrinks toward post-1967 territories only), settler_colonial generalizes the displacement analysis to 1948 itself, cultural_zionist detaches the claim from sovereignty, post_zionist targets the ethnic framework rather than the border.',
    'Classification is reading-indexed: the same standing arrangement computes as snare under this reading but as tangled_rope or rope under siblings. Cross-reading comparison, never within-story hedging, is the correct instrument.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: one reading among five of a contested kernel; sibling deltas recorded here rather than folded into this story.').

omega_variable(
    divine_title_naturalness,
    'Is the divine-title claim an irreducible theological given for its holders, or a modern political construction — activist religious nationalism accelerated after 1967 — that benefits identifiable movements?',
    'Doctrinal genealogy: trace pre-modern rabbinic quietism (the three-oaths tradition), Rav Kook''s inversion of waiting into action, and Gush Emunim''s conversion of territorial maximalism from awaited miracle into present obligation; locate when the claim became politically operative rather than liturgically preserved.',
    'If constructed, the claim forfeits transcendence-based immunity and competes as ordinary political interest; false-summit-style reclassification pressure rises and the beneficiary set becomes legible as rent-collecting rather than covenant-serving.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_title_naturalness, conceptual, 'Naturalness ambiguity: theological given versus constructed instrument with identifiable beneficiaries.').

omega_variable(
    victim_seat_epistemic_exclusion,
    'Does the reading''s subordination of the Palestinian seats reflect the arrangement''s actual cost structure, or the reading''s frame, which assigns those seats no counting weight?',
    'Compare epsilon and victim sets across the five sibling readings over the shared referent: convergence on high extraction despite divergent frames locates the extraction in the structure; divergence locates it in the frame.',
    'If frame-driven, this story''s effective extraction understates the payer-seat position and the family comparison is mandatory before any verdict is drawn from this file alone.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(victim_seat_epistemic_exclusion, conceptual, 'Whether the reading''s victim-calculus subordination is structural fact or epistemic exclusion.').

omega_variable(
    vanguard_identity_lock_mechanism,
    'Is the settlement vanguard''s inability to exit internalized theology, structural embeddedness (schools, salaries, army careers, municipal economies), or relational fusion with specific places?',
    'Post-evacuation trajectories of the 2005 Gaza displacement cohort: rapid re-fusion elsewhere indicates structural lock; persistent breakdown and returnism indicate internalized or relational lock.',
    'An internalized lock raises effective suppression above the structural measure and lowers the probability that political change alone unwinds the arrangement; a structural lock responds to compensation-and-relocation instruments.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vanguard_identity_lock_mechanism, empirical, 'Mechanism of the vanguard''s identity-locked exit.').

omega_variable(
    annexation_formalization_trajectory,
    'Will the de facto annexation regime be formalized — sovereignty-extension legislation, wholesale Area C absorption, external recognition — within the story''s forward horizon?',
    'Track Knesset sovereignty bills, settlement legalization statutes, and great-power recognition shifts as leading indicators.',
    'Formalization converts administrative extraction into titled extraction, deletes the last partition-exit fiction, and pushes the suppression requirement higher; failure to formalize leaves the arrangement in its current extralegal equilibrium.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(annexation_formalization_trajectory, empirical, 'Trajectory of de facto toward de jure annexation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__religious_zionist_reading, 1967, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1967, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 1967, 0.15).
narrative_ontology:measurement(jewi_tr_t1975, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 1975, 0.18).
narrative_ontology:measurement(jewi_tr_t1985, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 1985, 0.22).
narrative_ontology:measurement(jewi_tr_t1995, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 1995, 0.28).
narrative_ontology:measurement(jewi_tr_t2005, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 2005, 0.31).
narrative_ontology:measurement(jewi_tr_t2015, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 2015, 0.34).
narrative_ontology:measurement(jewi_tr_t2025, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 2025, 0.38).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1967, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 1967, 0.55).
narrative_ontology:measurement(jewi_be_t1975, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 1975, 0.62).
narrative_ontology:measurement(jewi_be_t1985, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 1985, 0.7).
narrative_ontology:measurement(jewi_be_t1995, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 1995, 0.72).
narrative_ontology:measurement(jewi_be_t2005, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 2005, 0.78).
narrative_ontology:measurement(jewi_be_t2015, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 2015, 0.84).
narrative_ontology:measurement(jewi_be_t2025, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 2025, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1967, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 1967, 0.45).
narrative_ontology:measurement(jewi_su_t1975, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 1975, 0.52).
narrative_ontology:measurement(jewi_su_t1985, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 1985, 0.58).
narrative_ontology:measurement(jewi_su_t1995, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 1995, 0.62).
narrative_ontology:measurement(jewi_su_t2005, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 2005, 0.74).
narrative_ontology:measurement(jewi_su_t2015, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 2015, 0.78).
narrative_ontology:measurement(jewi_su_t2025, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 2025, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__religious_zionist_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine__cultural_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine__post_zionist_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of kernel jewish_sovereignty_palestine per the epsilon-invariance principle: the colloquial label 'Jewish sovereignty in Palestine' conflates five structurally distinct claims — divine-title maximalism, self-determination partitionism, cultural-centrism, settler-colonial characterization, and post-national dissolution — each with its own epsilon, victim set, and classification over the same standing arrangement. This story is the maximal-extraction member: its title-claim admits no counterparty, no border, and no consent, whereas the liberal-nationalist sibling caps the claim at a negotiated line and the cultural sibling detaches it from sovereignty altogether. Influence runs both directions across the family: this reading cites the 1948 achievement as providential evidence while generating settlement facts that progressively destroy the liberal sibling's negotiability — the edges are declared from this story to each sibling accordingly.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
