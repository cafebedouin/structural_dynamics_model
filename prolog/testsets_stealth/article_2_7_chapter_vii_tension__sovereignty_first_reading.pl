% ============================================================================
% CONSTRAINT STORY: article_2_7_chapter_vii_tension__sovereignty_first_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_2_7_chapter_vii_tension__sovereignty_first_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: article_2_7_chapter_vii_tension__sovereignty_first_reading
 *   human_readable: Charter Sovereignty Shield: Article 2(7) Non-Intervention with Inter-State-Only Chapter VII Trigger
 *   domain: political/legal/international
 *
 * SUMMARY:
 *   The UN Charter's Article 2(7) guarantee of non-intervention, read
 *   together with a Chapter VII enforcement trigger confined to inter-state
 *   aggression, constitutes the sovereignty-first settlement of 1945: states
 *   are inviolable persons; force crosses borders only with the target's
 *   consent or nine Council votes free of permanent-member veto, and the
 *   lawful triggers center on aggression between states. This story
 *   instantiates the sovereignty_first_reading of the
 *   article_2_7_chapter_vii_tension kernel and authors the standing
 *   arrangement under contest — the sovereignty-first settlement itself —
 *   with epsilon assessed for that arrangement as the critical seat sees it:
 *   the same guarantee that protects weak states from predation converts
 *   domestic atrocity into a matter beyond lawful external reach, and the
 *   populations inside atrocity states bear the arrangement's cost with no
 *   compensating protection. The sibling reading (r2p_reading) is a separate
 *   constraint with its own epsilon, beneficiaries, and victims; the two
 *   files are linked through network.affects_constraints and neither folds
 *   the other into its classification. Claimed type and metrics are authored
 *   independently: the claim is tangled_rope because the arrangement
 *   possesses a genuine, still-live coordination function (mutual restraint
 *   among states) while operating asymmetric costs through the identical
 *   structure; the metrics describe the arrangement's observable operation
 *   across the Charter era. KEY AGENTS (by structural relationship): -
 *   permanent_five_members: Agenda setter (institutional/arbitrage) —
 *   administers the gate, collects immunity for themselves and clients -
 *   authoritarian_regimes: Primary beneficiary (powerful/constrained) — the
 *   shield converts internal atrocity into lawful domestic affair -
 *   postcolonial_state_governments: Secondary beneficiary
 *   (organized/constrained) — the shield is their principal defense against
 *   great-power predation - populations_under_domestic_atrocity: Primary
 *   target (powerless/trapped) — bear the arrangement's full cost with no
 *   compensating protection - humanitarian_ngo_community: Excluded voice
 *   (organized/constrained) — documents and protests from outside the chamber
 *   - international_law_scholars: Analytical observer (analytical/analytical)
 *   — maps the doctrinal terrain for both camps
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.72).
domain_priors:suppression_score(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.68).
domain_priors:theater_ratio(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_2_7_chapter_vii_tension__sovereignty_first_reading, tangled_rope).
narrative_ontology:human_readable(article_2_7_chapter_vii_tension__sovereignty_first_reading, "Charter Sovereignty Shield: Article 2(7) Non-Intervention with Inter-State-Only Chapter VII Trigger").
narrative_ontology:topic_domain(article_2_7_chapter_vii_tension__sovereignty_first_reading, "political/legal/international").

domain_priors:requires_active_enforcement(article_2_7_chapter_vii_tension__sovereignty_first_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_2_7_chapter_vii_tension__sovereignty_first_reading, '4fdb5cfa-5e7f-4eaa-93f8-31b6955203bc').
narrative_ontology:cs_kernel_codification('4fdb5cfa-5e7f-4eaa-93f8-31b6955203bc', fixed_text).
narrative_ontology:cs_authority_grounding('4fdb5cfa-5e7f-4eaa-93f8-31b6955203bc', extraction).
narrative_ontology:cs_interpretation_layer_present('4fdb5cfa-5e7f-4eaa-93f8-31b6955203bc').
narrative_ontology:cs_reading_relation('4fdb5cfa-5e7f-4eaa-93f8-31b6955203bc', article_2_7_chapter_vii_tension__r2p_reading, influences).
narrative_ontology:cs_axiom('4fdb5cfa-5e7f-4eaa-93f8-31b6955203bc', foundational, nonintervention_prevents_interstate_war).
narrative_ontology:cs_axiom_status(nonintervention_prevents_interstate_war, holdable).
narrative_ontology:cs_axiom_grounding('4fdb5cfa-5e7f-4eaa-93f8-31b6955203bc', nonintervention_prevents_interstate_war, instrumental).
narrative_ontology:cs_axiom('4fdb5cfa-5e7f-4eaa-93f8-31b6955203bc', foundational, domestic_jurisdiction_excludes_external_review).
narrative_ontology:cs_axiom_status(domestic_jurisdiction_excludes_external_review, holdable).
narrative_ontology:cs_axiom_grounding('4fdb5cfa-5e7f-4eaa-93f8-31b6955203bc', domestic_jurisdiction_excludes_external_review, conventional).
narrative_ontology:cs_reference_frame('4fdb5cfa-5e7f-4eaa-93f8-31b6955203bc', charter_sovereignty_compact).
narrative_ontology:cs_drift_state('4fdb5cfa-5e7f-4eaa-93f8-31b6955203bc', post_r2p_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('4fdb5cfa-5e7f-4eaa-93f8-31b6955203bc', '').
narrative_ontology:cs_kernel_id(article_2_7_chapter_vii_tension__sovereignty_first_reading, article_2_7_chapter_vii_tension).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__sovereignty_first_reading, authoritarian_regimes).
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__sovereignty_first_reading, postcolonial_state_governments).
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__sovereignty_first_reading, permanent_five_members).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__sovereignty_first_reading, populations_under_domestic_atrocity).
narrative_ontology:constraint_vindicates(article_2_7_chapter_vii_tension__sovereignty_first_reading, westphalian_nonintervention_doctrine).
narrative_ontology:constraint_vindicates(article_2_7_chapter_vii_tension__sovereignty_first_reading, council_gatekeeping_exclusivity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafted the Charter's enforcement architecture and each holds a veto over Chapter VII action. The gate they administer doubles as a shield: it insulates their own conduct and their clients from external review. They can and occasionally do act outside the framework when their interests align (coalitions of the willing), which is why leaving the arrangement costs them little — they arbitrage between its legality and their capabilities.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, permanent_five_members, agenda_setter,
    institutional, generational, arbitrage, global).

% Govern through mass violence against parts of their own population. The non-intervention guarantee converts their internal conduct into a matter beyond lawful external reach unless nine Council votes concur — votes their patrons or fellow veto-holders can block. Accepting conditionality on their sovereignty would threaten regime survival, so they defend the restrictive reading absolutely and bankroll coalitions of states that do the same.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, authoritarian_regimes, beneficiary,
    powerful, biographical, constrained, regional).

% Entered independence into a system whose sovereignty guarantee is their principal defense against great-power predation; without it, their territory and resources are negotiable at the margin of stronger states' grievances. They organize collectively (Non-Aligned Movement, G77) to defend the guarantee. They pay indirectly when a neighbor's government turns the same shield against its people: refugee flows, regional war, and the precedent that protection depends on great-power concurrence.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, postcolonial_state_governments, beneficiary,
    organized, generational, constrained, global).

% Face extermination, expulsion, or starvation administered by their own government. The same rule that bars foreign attack on their state bars anyone from entering to stop the killing without Council authorization their tormentor's allies can veto. Their exits are flight to borders that may be closed, hiding, or survival until the killing exhausts itself; they hold no vote, no seat, and no patron.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, populations_under_domestic_atrocity, payer,
    powerless, immediate, trapped, regional).

% Document atrocities, name victims, and lobby the Council and member states for action. They hold no seat in the chamber and no vote on its output; their advocacy is bounded by the legal framework they seek to amend, and their access to affected populations often depends on the goodwill of the very governments they implicate.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, humanitarian_ngo_community, excluded,
    organized, biographical, constrained, global).

% Map the doctrinal terrain: the text of Article 2(7), the scope of Chapter VII, the legality of unauthorized humanitarian intervention. They supply arguments to both camps and record state practice as it accumulates, but command no enforcement capacity and no vote.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, international_law_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_2_7_chapter_vii_tension__sovereignty_first_reading, authoritarian_regimes).
narrative_ontology:fixing_cost_class(article_2_7_chapter_vii_tension__sovereignty_first_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates mutual restraint among states: a hard presumption against cross-border force removes the pretext problem (every grievance otherwise licenses invasion), lets weak states exist without great-power patronage, and concentrates lawful force in a single gated channel requiring great-power concurrence.
% TRANSFER_FUNCTION: Moves security from intervening powers to incumbent governments — each state receives a guarantee against external force in exchange for extending the same guarantee to all others; the uncompensated cost falls on persons inside states whose rulers turn the shield against them.
% ABSENT_VOICES: Populations facing atrocity have no seat anywhere in the Charter architecture; they appear only through their governments — frequently the party destroying them. Humanitarian organizations and R2P-advocating states address the system from outside the chamber; victims' testimony enters, if at all, as material for others' arguments.
% DISAPPEARANCE_RATIONALE: If the sovereignty-first rule vanished overnight, every border would become negotiable at the margin of someone's grievance: weak states would rush into patronage blocs for protection, intervention doctrines would multiply and collide, and the eight-decade suppression of direct great-power war — however imperfectly achieved — would be up for renegotiation. Atrocity response would improve at the price of reopening the problem the arrangement was built to close.
% FOUNDING_PROBLEM: The catastrophic recurrence of interstate war culminating in the Second World War: the founders hard-coded non-intervention and concentrated enforcement in a great-power-concurring Council to make cross-border aggression legally exceptional and practically gated.
% FOUNDING_PROBLEM_CORROBORATION: Security-studies scholarship on the long peace attests the interstate-war-suppression function independent of any state beneficiary; the R2P movement's own architects concede the Charter order's interstate achievement and confine their quarrel to the domestic-atrocity case; the 2022 invasion of Ukraine demonstrated the founding problem's persistence to parties on every side of the reading dispute. Corroboration from outside the benefiting parties is substantial.
narrative_ontology:disappearance_verdict(article_2_7_chapter_vii_tension__sovereignty_first_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_2_7_chapter_vii_tension__sovereignty_first_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_2_7_chapter_vii_tension__sovereignty_first_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_2_7_chapter_vii_tension__sovereignty_first_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_2_7_chapter_vii_tension__sovereignty_first_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__sovereignty_first_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_2_7_chapter_vii_tension__sovereignty_first_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72 at interval end) because the arrangement's cost falls on a class of persons with no voice in its administration and no exit from its coverage: for a population facing state-run annihilation, the non-intervention guarantee is a categorical denial of rescue, and the veto makes the denial durable. Suppression (0.68) is the raw structural force maintaining the restrictive reading — veto deployment, diplomatic pressure against unauthorized intervention, doctrinal defense — and is authored unscaled, as a property of the arrangement rather than of any agent's position. Theater (0.48) reflects a growing share of performative activity: humanitarian debate in the chamber, commissions of inquiry, and never-again declarations that substitute for action, though the underlying restraint function remains real. Accessibility collapse is moderate (0.45): alternatives exist and are articulated (R2P doctrine, Uniting for Peace, unauthorized humanitarian intervention) but each is degraded in practice by the veto, by legality stigma, and by the Iraq-era poisoning of intervention credibility. Resistance (0.60) is sustained and organized: the R2P coalition, humanitarian organizations, and dissenting legal scholarship contest the reading continuously. The temporal series run on one shared grid (decade points 1945-2025) with all three metrics authored at every point. The suppression_requirement series is included because the story specifically tracks enforcement-capacity change: Cold War paralysis gave way to post-Cold War tests of the gate, the R2P challenge forced active doctrinal defense, and the Syria-era vetoes re-hardened enforcement — an enforcement history, not a static picture. The non-monotonic dips (1985, 2005) mark the Cambodian atrocity period's normalization and the R2P summit's partial mitigation respectively; the series is event-driven drift, not oscillation, so no cyclical mechanism is claimed.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the permanent-five seat the arrangement is a constitution working as designed: great-power concurrence before force, mutual immunity, eight decades without direct great-power war. From the postcolonial-government seat it is an indispensable shield whose known costs are borne elsewhere. From the atrocity-population seat the same structure is abandonment codified as law. From the scholarly seat it is a doctrinal tension awaiting resolution. The engine computes per-seat classifications from the structural data; the divergence among these readings of one arrangement is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (authoritarian_regimes, postcolonial_state_governments, permanent_five_members) derive low directionality — the arrangement subsidizes them, and for the atrocity-committing regimes it subsidizes precisely the conduct that generates the victims. The declared victim class (populations_under_domestic_atrocity) derives high directionality, amplified by trapped exit and powerless power: they cannot leave the state that harms them and cannot summon the force that could stop it. Global spatial scope raises verification difficulty and scales effective extraction upward for targets. Suppression is not scaled: it is the raw maintenance force of the veto-and-doctrine machinery. No directionality overrides are used — the beneficiary/victim declarations plus exit options already place every seat correctly, and the two institutional beneficiaries differ in role (agenda-setter versus passive collector) rather than in derived direction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — suppressing interstate war — is still live, so the arrangement is not mandatrophy-resolved and must not be read as a vestige. The mandatrophy risk runs in the opposite direction: as direct great-power war has receded, the arrangement's public justification has migrated toward functions it never claimed (order, stability, anti-imperial solidarity), while its costliest operation — shielding atrocity — is defended as the necessary price of the original function. Classifying as tangled_rope keeps both truths simultaneously: calling the arrangement a snare would erase the real protection weak states receive and hand its defenders a refutation; calling it a rope would erase the populations whose destruction it renders lawful. The mismatch consumer reads founding_problem_status (live) against disappearance_verdict (world_rearranges) and finds no zombie flag — the arrangement persists because its founding problem persists, not because its function died.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_delta,
    'This constraint is the sovereignty_first_reading of the article_2_7_chapter_vii_tension kernel; what structurally changes if the r2p_reading is adopted instead?',
    'Comparative authoring of the sibling story: diff the victim sets, intervention triggers, and beneficiary structures across the two readings; the engine''s per-seat classifications over both files locate where the disagreement carries structural weight.',
    'Under the r2p_reading, populations under atrocity move from unprotected victims to protected beneficiaries and defiant governments become intervention targets; this file''s high-extraction profile would invert. Classification of the shared kernel is therefore reading-relative, not kernel-relative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_delta, conceptual, 'Committer-frame delta between the sovereignty-first and R2P readings of the same Charter kernel.').

omega_variable(
    naturalness_of_sovereign_order,
    'Is sovereignty-as-foundational a natural feature of political order (an equilibrium any functioning interstate system converges on) or a constructed arrangement serving identifiable agents?',
    'Historical-comparative analysis of pre-Westphalian and non-Westphalian orders (empires, suzerainties, protectorates, pooled-sovereignty experiments) and of periods when intervention norms loosened without systemic collapse.',
    'If constructed, the arrangement is revisable policy and natural-law certification is unavailable; if natural, part of its persistence requires no enforcement and the measured suppression overstates maintenance cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalness_of_sovereign_order, conceptual, 'Natural-law versus constructed status of the sovereignty foundation.').

omega_variable(
    intervention_counterfactual_efficacy,
    'Would interventions permitted under the sibling reading actually improve outcomes for atrocity populations, or reproduce the pattern in which armed intervention deepens the harm it enters to stop?',
    'Systematic comparison of humanitarian-intervention outcomes (Sierra Leone, East Timor, Kosovo, the Libya 2011 aftermath) against non-intervention atrocity cases (Rwanda, Darfur, Syria) on civilian mortality and displacement measures.',
    'If interventions typically worsen outcomes, part of this arrangement''s measured cost is the price of avoiding greater harm and classification shifts toward rope; if they typically help, the blocked alternative''s value confirms the cost as a net loss imposed on victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intervention_counterfactual_efficacy, empirical, 'Whether the intervention this reading blocks would help or harm its intended beneficiaries.').

omega_variable(
    veto_design_or_capture,
    'Is the restrictive reading maintained because the great-power concert design requires it, or because particular permanent members exploit it to shield clients and their own conduct?',
    'Voting-record analysis across atrocity cases, correlating veto deployment with the vetoing member''s alliance ties to the targeting regime and with the vetoing member''s own exposure to analogous scrutiny.',
    'Design-based persistence supports the hybrid coordination/extraction reading; capture-dominant persistence indicates drift toward pure extraction — the same structure classified differently depending on whose interest the gate actually serves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_design_or_capture, empirical, 'Whether Council gatekeeping reflects constitutional design or member-specific capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_2_7_chapter_vii_tension__sovereignty_first_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1945, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 1945, 0.12).
narrative_ontology:measurement(arti_tr_t1955, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 1955, 0.16).
narrative_ontology:measurement(arti_tr_t1965, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 1965, 0.21).
narrative_ontology:measurement(arti_tr_t1975, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 1975, 0.27).
narrative_ontology:measurement(arti_tr_t1985, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 1985, 0.29).
narrative_ontology:measurement(arti_tr_t1995, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 1995, 0.37).
narrative_ontology:measurement(arti_tr_t2005, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 2005, 0.33).
narrative_ontology:measurement(arti_tr_t2015, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 2015, 0.44).
narrative_ontology:measurement(arti_tr_t2025, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 2025, 0.48).

% Extraction over time
narrative_ontology:measurement(arti_be_t1945, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 1945, 0.4).
narrative_ontology:measurement(arti_be_t1955, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 1955, 0.43).
narrative_ontology:measurement(arti_be_t1965, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 1965, 0.49).
narrative_ontology:measurement(arti_be_t1975, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 1975, 0.53).
narrative_ontology:measurement(arti_be_t1985, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 1985, 0.51).
narrative_ontology:measurement(arti_be_t1995, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 1995, 0.63).
narrative_ontology:measurement(arti_be_t2005, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 2005, 0.58).
narrative_ontology:measurement(arti_be_t2015, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 2015, 0.67).
narrative_ontology:measurement(arti_be_t2025, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 2025, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1945, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 1945, 0.35).
narrative_ontology:measurement(arti_su_t1955, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 1955, 0.38).
narrative_ontology:measurement(arti_su_t1965, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 1965, 0.43).
narrative_ontology:measurement(arti_su_t1975, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 1975, 0.46).
narrative_ontology:measurement(arti_su_t1985, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 1985, 0.44).
narrative_ontology:measurement(arti_su_t1995, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 1995, 0.53).
narrative_ontology:measurement(arti_su_t2005, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 2005, 0.59).
narrative_ontology:measurement(arti_su_t2015, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 2015, 0.65).
narrative_ontology:measurement(arti_su_t2025, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 2025, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_2_7_chapter_vii_tension__sovereignty_first_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_2_7_chapter_vii_tension__sovereignty_first_reading, article_2_7_chapter_vii_tension__r2p_reading).

% DUAL FORMULATION NOTE:
% Constraint family: article_2_7_chapter_vii_tension decomposes into two readings — sovereignty_first_reading (this file) and r2p_reading. Same kernel text, different victim sets and intervention triggers, and therefore different stable epsilon values. Each file links the other via affects_constraints; neither folds the other's content into its own classification (epsilon-invariance). The upstream/downstream asymmetry runs from this reading to the sibling: the sovereignty-first gate controls the authorization channel through which any R2P operation must pass.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
