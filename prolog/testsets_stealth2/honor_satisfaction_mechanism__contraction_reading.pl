% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_mechanism__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_mechanism__contraction_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: honor_satisfaction_mechanism__contraction_reading
 *   human_readable: Honor Satisfaction Mechanism - Contraction Reading (Duel Obligation Evacuated from Possibility Space)
 *   domain: historical sociology / legal history / normative systems
 *
 * SUMMARY:
 *   For two centuries the armed gentry of Europe operated a satisfaction
 *   mechanism: an insult unanswered in arms destroyed the insulted man's
 *   standing, so gentlemen killed each other by protocol. The arrangement
 *   presented itself as the guardian of honor; it functioned as a
 *   class-boundary machine that charged its youngest and poorest members in
 *   blood. This story instantiates the contraction reading of the
 *   honor_satisfaction_mechanism kernel: the claim that the mechanism's end
 *   was neither suppression nor gradual fringe decay but category-shift - the
 *   possibility of settling honor by private combat dropped out of the
 *   gentleman's action space entirely, and enforcement machinery was
 *   dismantled afterward because there was nothing left to enforce. Per the
 *   epsilon-referent rule, extractiveness is authored for the standing
 *   arrangement under contest (the operative satisfaction regime) as this
 *   reading assesses it; the terminal collapse lives in the measurement
 *   series, not in the scalar. Sibling readings - decline (persistence at
 *   falling frequency) and composite (plural terminating mechanisms) - are
 *   separate stories; this file links them and does not average over them.
 *   KEY AGENTS (by structural relationship): - junior_officers: primary
 *   targets (powerless/trapped) - supply the blood that authenticates the
 *   code - senior_officer_hierarchy: principal collectors
 *   (powerful/identity_locked) - administer the courts their authority rests
 *   on - aristocratic_officer_class: collective beneficiary
 *   (organized/constrained) - collects the boundary, distributes its costs
 *   downward - metropolitan_gentlemanly_society: secondary beneficiary
 *   (organized/mobile) - enforces vicariously, consumes the spectacle, bears
 *   no mortal risk - honorable_refusers: principled cost-bearers
 *   (moderate/constrained) - pay in ostracism; their vindication marks the
 *   repricing - duel_widows_and_orphans: uncompensated cost-bearers
 *   (powerless/trapped) - no seat in any court of honor -
 *   state_legal_authorities: external agenda-setter (institutional/mobile) -
 *   prohibits for two centuries, ratifies the accomplished fact at the end -
 *   historians_of_honor: analytical observers - hold the timing evidence the
 *   termination question turns on
 *
 * KEY AGENTS:
 *   - junior_officers: primary target (powerless/trapped) - bears the mechanism's mortal and career costs
 *   - senior_officer_hierarchy: principal collector and administrator (powerful/identity_locked) - authority underwritten by the code it enforces
 *   - aristocratic_officer_class: collective beneficiary (organized/constrained) - status boundary maintained at members' individual expense
 *   - metropolitan_gentlemanly_society: secondary beneficiary (organized/mobile) - enforces vicariously from clubland and press
 *   - honorable_refusers: principled dissenters (moderate/constrained) - bear sustained ostracism; their normalization marks the repricing
 *   - duel_widows_and_orphans: uncompensated cost-bearers (powerless/trapped) - no seat in any court of honor
 *   - state_legal_authorities: external agenda-setter (institutional/mobile) - two centuries of failed prohibition, then ratification of the accomplished fact
 *   - historians_of_honor: analytical observers (analytical/analytical) - reconstruct operation and termination from archives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_mechanism__contraction_reading, 0.62).
domain_priors:suppression_score(honor_satisfaction_mechanism__contraction_reading, 0.82).
domain_priors:theater_ratio(honor_satisfaction_mechanism__contraction_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_mechanism__contraction_reading, tangled_rope).
narrative_ontology:human_readable(honor_satisfaction_mechanism__contraction_reading, "Honor Satisfaction Mechanism - Contraction Reading (Duel Obligation Evacuated from Possibility Space)").
narrative_ontology:topic_domain(honor_satisfaction_mechanism__contraction_reading, "historical sociology / legal history / normative systems").

domain_priors:requires_active_enforcement(honor_satisfaction_mechanism__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_mechanism__contraction_reading, '91d16ba5-a148-4401-b09c-af5bece104b9').
narrative_ontology:cs_kernel_codification('91d16ba5-a148-4401-b09c-af5bece104b9', formalized).
narrative_ontology:cs_authority_grounding('91d16ba5-a148-4401-b09c-af5bece104b9', practice).
narrative_ontology:cs_interpretation_layer_present('91d16ba5-a148-4401-b09c-af5bece104b9').
narrative_ontology:cs_reading_relation('91d16ba5-a148-4401-b09c-af5bece104b9', honor_satisfaction_mechanism__decline_reading, coexists_with).
narrative_ontology:cs_reading_relation('91d16ba5-a148-4401-b09c-af5bece104b9', honor_satisfaction_mechanism__composite_reading, influences).
narrative_ontology:cs_axiom('91d16ba5-a148-4401-b09c-af5bece104b9', foundational, category_shift_termination).
narrative_ontology:cs_axiom_status(category_shift_termination, holdable).
narrative_ontology:cs_axiom_grounding('91d16ba5-a148-4401-b09c-af5bece104b9', category_shift_termination, empirically_contingent).
narrative_ontology:cs_axiom('91d16ba5-a148-4401-b09c-af5bece104b9', secondary, compliance_preceded_enforcement_collapse).
narrative_ontology:cs_axiom_status(compliance_preceded_enforcement_collapse, holdable).
narrative_ontology:cs_axiom_grounding('91d16ba5-a148-4401-b09c-af5bece104b9', compliance_preceded_enforcement_collapse, empirically_contingent).
narrative_ontology:cs_reference_frame('91d16ba5-a148-4401-b09c-af5bece104b9', obligatory_honor_satisfaction_regime).
narrative_ontology:cs_drift_state('91d16ba5-a148-4401-b09c-af5bece104b9', interwar_demobilization_era, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('91d16ba5-a148-4401-b09c-af5bece104b9', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_mechanism__contraction_reading, honor_satisfaction_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__contraction_reading, aristocratic_officer_class).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__contraction_reading, senior_officer_hierarchy).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__contraction_reading, metropolitan_gentlemanly_society).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__contraction_reading, junior_officers).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__contraction_reading, honorable_refusers).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__contraction_reading, duel_widows_and_orphans).
narrative_ontology:constraint_vindicates(honor_satisfaction_mechanism__contraction_reading, point_of_honor_doctrine).
narrative_ontology:constraint_vindicates(honor_satisfaction_mechanism__contraction_reading, satisfaction_capacity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A trans-European status corporation whose standing rested on demonstrated personal courage. The satisfaction code policed its boundary: membership required willingness to answer insults in arms, and the class collected the resulting reputational exclusivity. Individual members paid the code's costs - deaths, ruined careers, coerced challenges - while the collective good, a verifiable gentlemanly caste line against bourgeois imitation, accrued to all. Leaving the class meant surrendering the estate, commission, and marriage-market position that structured members' entire lives.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, aristocratic_officer_class, beneficiary,
    organized, generational, constrained, continental).

% Generals and colonels wrote and administered the dueling regulations, presided over courts of honor, and decided which insults demanded satisfaction. Their command authority was underwritten by the same code they enforced: a senior officer's word carried weight because his honor stood established beyond question. Guardianship of the code fused with their professional self-conception - administering it was not a task they performed but what they were. When the code dissolved, this seat dissolved with it.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, senior_officer_hierarchy, beneficiary,
    powerful, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_mechanism__contraction_reading, senior_officer_hierarchy, agenda_setter).

% Clubs, salons, and the political press consumed the code's outputs: admission committees required satisfactory accounts of any member's conduct in affairs of honor, and newspapers treated challenges as front-page civic events. Society enforced attendance norms vicariously - a man who refused satisfaction found his club invitations drying up - while bearing almost none of its mortal risks. Its investment was portable: as the honor economy faded, the same clubs and papers re-priced status around sport, wealth, and wit.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, metropolitan_gentlemanly_society, beneficiary,
    organized, generational, mobile, national).

% Subalterns and lieutenants bore the code's mortal arithmetic. A superior's displeasure or a peer's taunt could obligate them onto a pistol range within days; refusal meant resignation, scandal, and exclusion from the profession they had been bred for since childhood. They did not set the rules, staff the courts, or collect the reputation - they supplied the blood that authenticated it. Exit routes existed (resignation, colonial service, emigration) but each cost the entire invested life.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, junior_officers, payer,
    powerless, immediate, trapped, national).

% Officers and gentlemen - disproportionately evangelical, Quaker, or rationalist - who declined to recognize the satisfaction obligation on principle. They paid in sustained coin: courts of honor recorded them as dishonored, promotions stalled, clubs blacklisted them, and the press rehearsed their disgrace. Their ranks grew across the nineteenth century, and their eventual celebration - refusers honored rather than ruined - is one of the clearest markers that the underlying price list had been rewritten.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, honorable_refusers, payer,
    moderate, biographical, constrained, national).

% Families of men killed in satisfaction encounters. They inherited the loss without ever having been parties to the obligation: no widow sat in a court of honor, no orphan's claim weighed against a regiment's point of honor. Pensions were discretionary and scandal-adjacent; the code's records treated their men as having died honorably, which settled the matter administratively while leaving the household destitute.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, duel_widows_and_orphans, payer,
    powerless, generational, trapped, local).

% Sovereigns, legislatures, and courts prohibited the practice repeatedly - edicts from the sixteenth century onward, criminal statutes, army regulations threatening cashiering - and failed for two hundred years: prosecutions collapsed for lack of witnesses, juries refused to convict, and officers treated nominal sentences as career noise. The state bore the governance cost of an obligation it could not reach inside the corps. When the practice finally collapsed, the same authorities repealed or shelved the old army dueling codes with essentially no opposition - ratifying an accomplished fact rather than achieving one.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, state_legal_authorities, agenda_setter,
    institutional, generational, mobile, national).

% Reconstruct the mechanism's operation and termination from regimental archives, court records, correspondence, and press. They supply the timing evidence on which the termination question turns - when compliance collapsed relative to enforcement - and they disagree among themselves along exactly the lines the sibling readings mark.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, historians_of_honor, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_satisfaction_mechanism__contraction_reading, senior_officer_hierarchy).
narrative_ontology:fixing_cost_class(honor_satisfaction_mechanism__contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Managed three collective-action problems for the armed gentry: it replaced open vendetta with bounded, consensual, ritualized combat (a killing licensed by both sides' consent stopped the feud cycle); it manufactured a costly, hard-to-fake signal of courage on which a class whose power rested on unverifiable bravery claims could rely; and it disciplined rank inside regiments by making every officer's standing hostage to conduct the hierarchy could adjudicate.
% TRANSFER_FUNCTION: Moved life, limb, and career security from individual junior members upward into the collective status capital of the class: each death or disgrace purchased a marginal increment of boundary credibility for everyone else. It also transferred the right to shrug off an insult from the individual (who might prefer to laugh) to the code (which decided what demanded satisfaction).
% ABSENT_VOICES: Duel widows and orphans had no seat in any court of honor and no standing in the satisfaction calculus; enlisted soldiers, barred from giving or demanding satisfaction yet dying in its shadow, had none either; refusers spoke only as defendants in the proceedings that condemned them. Women enforced the code from the salon while being structurally incapable of resorting to it.
% DISAPPEARANCE_RATIONALE: Regimental sociability, promotion politics, club admissions, parliamentary courage-talk, and the protocol of insult all reorganized once the possibility of settling honor by private combat dropped out of the action space: apology norms professionalized, courage claims migrated to sporting and battlefield credentials, and the officer corps rebuilt its boundary on examinations and technical competence. Nothing reinstated the practice anywhere once cognition had shifted - including in states that had spent two centuries failing to suppress it by force.
% FOUNDING_PROBLEM: How an armed hereditary nobility manages its own internal violence without feud spirals, and how a class whose authority rests on personal courage makes that courage publicly verifiable and reliably priced.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the benefiting parties by two centuries of state prosecutorial records documenting the practice's ungovernability and costs, church condemnations, anti-dueling society pamphleteering, and military casualty accounting; in the historiography, Kiernan, Frevert, and Nye - working from archival seats outside any beneficiary set - locate the founding problem's dissolution in the replacement of the warrior-gentry by credentialed professional militaries. No surviving beneficiary party attests the problem's liveness; the class that would attest it no longer exists as a class.
narrative_ontology:disappearance_verdict(honor_satisfaction_mechanism__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_mechanism__contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_mechanism__contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_satisfaction_mechanism__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_mechanism__contraction_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_mechanism__contraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_satisfaction_mechanism__contraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_satisfaction_mechanism__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base-property scalars characterize the mechanism during its operative period (roughly the interval's first two-thirds), because that operative regime is the standing arrangement under contest this reading assesses; the measurement series then traces the contraction the reading is named for. Extractiveness 0.62: the obligation charged individual members mortal and career costs grossly disproportionate to any individually-collected return, while returning real collective goods - substantial but not total. Suppression 0.82, unscaled by scope or power per the framework's rule: the enforcement stack (courts of honor, ostracism, promotion gates, cashiering threats) was heavy, and the internalized-versus-structural split is carried by omega rather than folded into the scalar. Theater 0.25 in operation: challenges were real, deaths were real; ritual framed but did not replace function. Accessibility collapse 0.68: inside the honor economy the alternatives to satisfaction (apology without satisfaction, legal recourse) were themselves coded dishonorable, though exit from the economy altogether remained available at ruinous price - well short of natural-law completeness. Resistance 0.52: church, state, and dissenting conscience contested the code continuously for two centuries without displacing it - persistent, organized, ineffective. The series runs on one shared grid (T=0 to 220 in 40-year steps plus the terminal point): extractiveness peaks at T=120 with the Restoration-era intensification, then collapses to 0.06 as the obligation stops binding; suppression_requirement peaks alongside it and decays to 0.08 - the reading's signature is that enforcement decayed because compliance had become automatic, not the reverse; theater_ratio climbs to 0.65 as the form outlives the function, and the story owns that late theater honestly (see mandatrophy_analysis and the successor-status omega). Receipt surface: gain_flow names senior_officer_hierarchy because the extraction demonstrably accrued to the seat that administered the courts and whose command authority the code underwrote - the class benefits diffusely, but concentrated receipt sits with the hierarchy; receipt is authored separately from beneficiary-role. fixing_cost is authored prohibitive: for two centuries no lever-holder, state or class, could remove the mechanism at acceptable cost - state bans failed and internal abandonment was unavailable from inside the honor economy; removal became cheap only after the category shift, which is consequence, not cause.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute divergent classifications from identical structural data. From the junior-officer seat the arrangement is enforced extraction with lethal stakes and no exit; from the senior-hierarchy seat it is sacred administrative duty, maintenance of the only currency its authority trades in; from the class seat it is insurance whose premiums others pay; from the state seat it is an ungovernable nuisance that humiliated its statutes for two hundred years; from the refuser seat it is persecution that aged into vindication. Same code, five phenomenologies - the engine derives this divergence from power, exit, and role; nothing in the authored claim adjudicates it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the collector seats toward the subsidized end: the officer class (constrained exit - the class is the asset), the senior hierarchy (identity_locked - guardianship fused with self-conception), and metropolitan society (mobile - its consumption of the code was portable) all derive low d. Victim declarations drive the paying seats toward the full-target end: junior officers (trapped - every exit costs the whole invested life), refusers (constrained - they exited morally and paid socially for decades), and widows and orphans (trapped, powerless - maximal d at minimal power; the coalition question is moot because they were never assembled in any forum). One override: the institutional power atom carries d=0.4 for the state-legal seat. The derivation chain's canonical fallback would misplace an undeclared agenda-setting actor; the state's actual relationship was adversarial-without-relief - two centuries of failed prohibition made the mechanism a standing governance cost and a standing rebuke to its monopoly claims, placing it moderately toward the target end, though it never bore the code's mortal charges.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem died before the practice did: by the late nineteenth century the warrior-gentry was dissolving into credentialed professionalism, and the satisfaction mandate was defending a boundary whose economic base had moved. The R5 mismatch (status=dead x verdict=world_rearranges) therefore fires the zombie flag, and the story owns it: the zombie window is real but narrow - roughly 1870 to 1914, when the form persisted on momentum and spectacle after its function had departed. What prevents misclassification is the shape of the series: an inertial remnant holds theater high while extraction plateaus and nothing terminates it; here theater rises WHILE extraction and enforcement collapse together, and the terminal points show even the theater draining away after the 1914-1918 rupture. The repeal wave that followed cost its sponsors nothing - fixing became cheap only after cognition had shifted, which is the reading's central contention: cheapness was the consequence of dissolution, never its cause. Classifying the mechanism as a tangled rope in operation, with a fully evacuated terminal state, keeps the coordination function (vendetta-suppression, courage-verification, rank discipline) on the books without letting it launder the asymmetric mortality the coordination rode on.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    termination_mechanism_attribution,
    'Did the honor-satisfaction obligation terminate by internal category-shift (this reading: dueling became cognitively unthinkable) or by external suppression and gradual attrition (the terrain of the sibling readings)?',
    'Timing discrimination on the enforcement-compliance sequence: if willing compliance and challenge-frequency collapsed BEFORE enforcement machinery was dismantled, with correspondence showing the option dropping out of deliberation rather than being weighed and refused, the contraction signature holds; if prosecutions and disciplinary pressure ground the practice down against persistent willingness, the decline and composite accounts gain. Proxies: silence about challenges in correspondence where earlier generations negotiated them routinely; absence of clandestine dueling despite intact incentive (a suppressed practice hides; an unthinkable one vanishes).',
    'If suppression-driven, this reading''s terminal collapse is misattributed: the arrangement better models as persistent-at-fringe (decline) or multi-mechanism (composite), and the terminal measurements here overstate the evacuation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(termination_mechanism_attribution, empirical, 'Which mechanism terminated the honor-satisfaction obligation: category-shift, suppression, or attrition.').

omega_variable(
    internalized_vs_structural_compulsion,
    'Was the operating-phase compulsion to give satisfaction primarily structural (career penalty, ostracism machinery, courts of honor) or internalized (honor ideology making refusal inconceivable from inside)?',
    'Content analysis of memoirs, court-martial records of refusers, and chaplain and medical reports: refusers who report calculating penalties indicate structural compulsion; refusers who report being unable to regard refusal as a live option indicate internalization; the ratio tracks the split.',
    'If predominantly internalized, the scalar suppression understates the mechanism''s grip, and the population was pre-adapted for the category-shift termination, softening this reading''s discontinuity claim (the shift completed an internal process rather than replacing an external one); if predominantly structural, the termination is genuinely discontinuous: external machinery dissolved and nothing internal remained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_compulsion, empirical, 'Structural versus internalized share of the satisfaction compulsion.').

omega_variable(
    terminal_theater_successor_status,
    'Does late-period theatrical dueling (bloodless epee affairs, academic fencing cultures retaining scar-marking) constitute residue of this arrangement, sustaining the decline reading, or successor practice detached from the satisfaction function, preserving this reading''s clean-termination claim?',
    'Functional analysis of the late forms: do they respond to insult and discharge satisfaction obligations (residue), or serve sport, identity-marking, and spectatorship with no satisfaction semantics (successor)? Test whether participants would recognize a satisfaction debt as the operative frame.',
    'If residue, ''evacuated from possibility space'' overclaims: the arrangement persisted in hollowed form and the terminal theater_ratio here measures a vestige, pushing the story toward inertial-remnant territory; if successor, the theater belongs to a different constraint and this story''s terminal collapse is clean.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(terminal_theater_successor_status, conceptual, 'Whether late theatrical dueling is residue of the mechanism or a detached successor practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_mechanism__contraction_reading, 0, 220).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hsm_contraction_tr_t0, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(hsm_contraction_tr_t0, observed).
narrative_ontology:measurement(hsm_contraction_tr_t40, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement_basis(hsm_contraction_tr_t40, observed).
narrative_ontology:measurement(hsm_contraction_tr_t80, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 80, 0.2).
narrative_ontology:measurement_basis(hsm_contraction_tr_t80, observed).
narrative_ontology:measurement(hsm_contraction_tr_t120, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 120, 0.28).
narrative_ontology:measurement_basis(hsm_contraction_tr_t120, observed).
narrative_ontology:measurement(hsm_contraction_tr_t160, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 160, 0.38).
narrative_ontology:measurement_basis(hsm_contraction_tr_t160, observed).
narrative_ontology:measurement(hsm_contraction_tr_t200, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 200, 0.55).
narrative_ontology:measurement_basis(hsm_contraction_tr_t200, observed).
narrative_ontology:measurement(hsm_contraction_tr_t220, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 220, 0.65).
narrative_ontology:measurement_basis(hsm_contraction_tr_t220, observed).

% Extraction over time
narrative_ontology:measurement(hsm_contraction_be_t0, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(hsm_contraction_be_t0, observed).
narrative_ontology:measurement(hsm_contraction_be_t40, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement_basis(hsm_contraction_be_t40, observed).
narrative_ontology:measurement(hsm_contraction_be_t80, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 80, 0.6).
narrative_ontology:measurement_basis(hsm_contraction_be_t80, observed).
narrative_ontology:measurement(hsm_contraction_be_t120, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 120, 0.66).
narrative_ontology:measurement_basis(hsm_contraction_be_t120, observed).
narrative_ontology:measurement(hsm_contraction_be_t160, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 160, 0.62).
narrative_ontology:measurement_basis(hsm_contraction_be_t160, observed).
narrative_ontology:measurement(hsm_contraction_be_t200, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 200, 0.45).
narrative_ontology:measurement_basis(hsm_contraction_be_t200, observed).
narrative_ontology:measurement(hsm_contraction_be_t220, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 220, 0.06).
narrative_ontology:measurement_basis(hsm_contraction_be_t220, observed).

% Suppression requirement over time
narrative_ontology:measurement(hsm_contraction_su_t0, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement_basis(hsm_contraction_su_t0, observed).
narrative_ontology:measurement(hsm_contraction_su_t40, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 40, 0.78).
narrative_ontology:measurement_basis(hsm_contraction_su_t40, observed).
narrative_ontology:measurement(hsm_contraction_su_t80, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 80, 0.8).
narrative_ontology:measurement_basis(hsm_contraction_su_t80, observed).
narrative_ontology:measurement(hsm_contraction_su_t120, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 120, 0.84).
narrative_ontology:measurement_basis(hsm_contraction_su_t120, observed).
narrative_ontology:measurement(hsm_contraction_su_t160, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 160, 0.72).
narrative_ontology:measurement_basis(hsm_contraction_su_t160, observed).
narrative_ontology:measurement(hsm_contraction_su_t200, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 200, 0.5).
narrative_ontology:measurement_basis(hsm_contraction_su_t200, observed).
narrative_ontology:measurement(hsm_contraction_su_t220, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 220, 0.08).
narrative_ontology:measurement_basis(hsm_contraction_su_t220, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_mechanism__contraction_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__contraction_reading, honor_satisfaction_mechanism__decline_reading).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__contraction_reading, honor_satisfaction_mechanism__composite_reading).

% DUAL FORMULATION NOTE:
% One colloquial label - 'the end of dueling' - decomposes into three structurally distinct claims with different epsilon profiles and terminal states: contraction (obligation evacuated from possibility space; terminal extraction near zero), decline (practice persists at fringe frequency; terminal extraction positive), composite (termination produced by interacting mechanisms). The contraction reading stands upstream of composite in one respect: its category-shift evidence is the component composite must weight or explain away; it stands in direct rivalry with decline on the terminal-state description. All three share the referent - the standing honor-satisfaction arrangement - and differ in assessment, keeping epsilon reading-indexed over a fixed object.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_satisfaction_mechanism__contraction_reading, institutional, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
