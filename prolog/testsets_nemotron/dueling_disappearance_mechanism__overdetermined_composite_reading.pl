% ============================================================================
% CONSTRAINT STORY: dueling_disappearance_mechanism__overdetermined_composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dueling_disappearance_mechanism__overdetermined_composite_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: dueling_disappearance_mechanism__overdetermined_composite_reading
 *   human_readable: Dueling's Disappearance — Overdetermined Composite Mechanism
 *   domain: historical_sociology/cultural_anthropology/legal_history
 *
 * SUMMARY:
 *   This constraint story models dueling's disappearance in the United States
 *   (1776-1890) as an overdetermined composite: four independent sufficient
 *   conditions — legal prohibition (state anti-dueling statutes from 1799
 *   onward), institutional modernization (professional military, commercial
 *   courts, bourgeois professions), cultural shift (dignity culture
 *   displacing honor culture via evangelicalism, sentimentalism, market
 *   integration), and Civil War trauma (750,000 deaths discrediting honor
 *   violence) — each sufficient to undermine dueling, operating
 *   simultaneously. No single mechanism dominates; the constraint's
 *   persistence requires all four suppressors to fail, which they do not. The
 *   reading is instantiated from the contested kernel
 *   'dueling_disappearance_mechanism' as the
 *   overdetermined_composite_reading. Sibling readings are
 *   contraction_reading (cultural displacement alone) and
 *   institutional_displacement_reading (institutional substitution alone).
 *   This reading treats the non-separability of causal pathways as
 *   structurally definitive: ε is not measurable per pathway because the
 *   pathways are not independent in their effects — they reinforce and
 *   complete each other.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.62).
domain_priors:suppression_score(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.48).
domain_priors:theater_ratio(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, resistance, 0.41).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__overdetermined_composite_reading, tangled_rope).
narrative_ontology:human_readable(dueling_disappearance_mechanism__overdetermined_composite_reading, "Dueling's Disappearance — Overdetermined Composite Mechanism").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__overdetermined_composite_reading, "historical_sociology/cultural_anthropology/legal_history").

domain_priors:requires_active_enforcement(dueling_disappearance_mechanism__overdetermined_composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__overdetermined_composite_reading, '91b38238-10b0-4475-b706-ec79b3fafc07').
narrative_ontology:cs_kernel_codification('91b38238-10b0-4475-b706-ec79b3fafc07', implicit).
narrative_ontology:cs_authority_grounding('91b38238-10b0-4475-b706-ec79b3fafc07', distributed).
narrative_ontology:cs_reading_relation('91b38238-10b0-4475-b706-ec79b3fafc07', dueling_disappearance_mechanism__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('91b38238-10b0-4475-b706-ec79b3fafc07', dueling_disappearance_mechanism__institutional_displacement_reading, coexists_with).
narrative_ontology:cs_axiom('91b38238-10b0-4475-b706-ec79b3fafc07', foundational, causal_overdetermination_is_structural).
narrative_ontology:cs_axiom_status(causal_overdetermination_is_structural, holdable).
narrative_ontology:cs_axiom_grounding('91b38238-10b0-4475-b706-ec79b3fafc07', causal_overdetermination_is_structural, empirically_contingent).
narrative_ontology:cs_axiom('91b38238-10b0-4475-b706-ec79b3fafc07', foundational, no_single_sufficient_condition_dominates).
narrative_ontology:cs_axiom_status(no_single_sufficient_condition_dominates, holdable).
narrative_ontology:cs_axiom_grounding('91b38238-10b0-4475-b706-ec79b3fafc07', no_single_sufficient_condition_dominates, empirically_contingent).
narrative_ontology:cs_reference_frame('91b38238-10b0-4475-b706-ec79b3fafc07', postcolonial_authority_vacuum).
narrative_ontology:cs_drift_state('91b38238-10b0-4475-b706-ec79b3fafc07', postbellum_institutional_consolidation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('91b38238-10b0-4475-b706-ec79b3fafc07', '').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__overdetermined_composite_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, state_legal_monopoly).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, professional_military_officer_corps).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, commercial_banking_interests).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, bourgeois_professional_classes).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, postwar_reconstruction_governments).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__overdetermined_composite_reading, aristocratic_officer_class).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__overdetermined_composite_reading, southern_planter_elite).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__overdetermined_composite_reading, honor_culture_practitioners).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__overdetermined_composite_reading, frontier_male_populations).
narrative_ontology:constraint_vindicates(dueling_disappearance_mechanism__overdetermined_composite_reading, state_monopoly_on_legitimate_violence).
narrative_ontology:constraint_vindicates(dueling_disappearance_mechanism__overdetermined_composite_reading, contract_law_supersedes_personal_honor).
narrative_ontology:constraint_vindicates(dueling_disappearance_mechanism__overdetermined_composite_reading, professional_meritocracy_over_birth_status).
narrative_ontology:constraint_vindicates(dueling_disappearance_mechanism__overdetermined_composite_reading, civil_society_dispute_resolution_via_courts).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts and enforces anti-dueling statutes; uses criminal prosecution of duelists and seconds to establish state monopoly on legitimate violence. Benefits from elimination of private violence that challenges state authority. Has no exit — it is the enforcement apparatus itself.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, state_legal_monopoly, agenda_setter,
    institutional, generational, arbitrage, national).

% West Point and Annapolis graduates push officer professionalization; dueling is banned in service regulations (1838 Articles of War). The corps benefits by replacing honor violence with merit-based promotion and courts-martial discipline. Exit is arbitrage — they designed the substitute system.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, professional_military_officer_corps, beneficiary,
    institutional, generational, arbitrage, national).

% Antebellum credit system depends on contract enforcement, not personal honor guarantees. Dueling's decline enables impersonal commercial law. Banking interests lobby for creditor-friendly courts. Mobile exit — capital moves to jurisdictions with reliable contract enforcement.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, commercial_banking_interests, beneficiary,
    powerful, biographical, mobile, national).

% Lawyers, doctors, merchants, editors gain status as dueling's aristocratic gatekeeping collapses. Professional associations (bar associations, medical societies) substitute formal credentialing for honor-based exclusion. Mobile exit — professional licenses portable across state lines.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, bourgeois_professional_classes, beneficiary,
    organized, biographical, mobile, national).

% Reconstruction regimes (1865-1877) enforce anti-dueling laws to dismantle planter elite's extrajudicial authority. They benefit from monopolizing violence in the South. Constrained exit — federal troop withdrawal ends their enforcement capacity.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, postwar_reconstruction_governments, agenda_setter,
    institutional, immediate, constrained, regional).
narrative_ontology:stakeholder_secondary_role(dueling_disappearance_mechanism__overdetermined_composite_reading, postwar_reconstruction_governments, beneficiary).

% Pre-professionalization officers (War of 1812, Mexican War veterans) whose status depends on willingness to duel. West Point curriculum initially taught dueling etiquette. Identity-locked — their self-concept as gentlemen-officers fuses with the duel; professionalization renders their honor capital worthless.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, aristocratic_officer_class, payer,
    organized, biographical, identity_locked, national).

% Planter class uses dueling to enforce racial hierarchy, political discipline, and commercial honor among peers. Civil War destroys their economic base; Reconstruction criminalizes their enforcement tool. Trapped — no exit from the collapsing social order; identity fused with mastery and honor.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, southern_planter_elite, payer,
    powerful, generational, trapped, regional).

% Non-elite white men in South and frontier for whom dueling/rough-and-tumble fighting is primary dispute resolution. Cultural shift to dignity culture (commerce, evangelicalism, courts) devalues their honor capital. Constrained exit — can migrate west but carry the culture; courts follow settlement.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, honor_culture_practitioners, payer,
    moderate, biographical, constrained, regional).

% Miners, cowboys, settlers in territories where formal law is absent. Dueling and informal violence are only dispute resolution. Territorial courts and vigilance committees gradually substitute. Trapped — no state protection; violence is survival. Excluded from the legislative process that criminalizes their only mechanism.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, frontier_male_populations, payer,
    powerless, immediate, trapped, regional).
narrative_ontology:stakeholder_secondary_role(dueling_disappearance_mechanism__overdetermined_composite_reading, frontier_male_populations, excluded).

% Analyzes the overdetermined causal structure from retrospective distance. Sees four sufficient conditions operating simultaneously: legal prohibition (statutes 1799-1880s), institutional modernization (courts, military professionalization, banking), cultural shift (dignity culture via evangelicalism, commerce, sentimentalism), Civil War trauma (mass death discredits honor violence). No stake in any mechanism's primacy.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, historical_sociologist, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Multiple overlapping coordination problems solved simultaneously: (1) state establishes monopoly on legitimate violence, (2) professional military substitutes merit discipline for honor violence, (3) commercial law replaces personal honor as credit foundation, (4) bourgeois professions substitute credentialing for aristocratic exclusion, (5) post-Civil War order dismantles planter extrajudicial authority.
% TRANSFER_FUNCTION: Moves dispute-resolution authority and status-allocation power from personal honor violence to state courts, professional licensing bodies, commercial contract law, and military merit systems. Transfers the capacity to compel compliance from private violence to public institutions. From aristocratic/planter elites to state, professions, and commercial interests.
% ABSENT_VOICES: Enslaved people — whose bodies were the stakes of planter honor duels and whose freedom made the planter honor system obsolete — never had a seat. Indigenous nations — whose treaty violations were enforced by duel-honed military officers — were excluded from the courts that replaced dueling. Women — for whom honor culture structured marriage, reputation, and legal disability — had no voice in the legislative criminalization of the system that governed them.
% DISAPPEARANCE_RATIONALE: If dueling and its four sufficient-condition suppressors vanished simultaneously, the entire dispute-resolution and status-allocation architecture of 19th-century America would need reconstruction: no state violence monopoly, no professional military discipline, no commercial contract enforcement, no bourgeois credentialing, no Reconstruction state authority. The world rearranges because dueling's disappearance IS the rearrangement — the substitute institutions are the constraint's persistence mechanism.
% FOUNDING_PROBLEM: The founding problem was not dueling itself but the vacuum of legitimate authority in post-colonial North America: no monopoly on violence, no reliable contract enforcement, no professional officer corps, no bourgeois status ladder, no mechanism to resolve disputes between equals without private violence. Dueling filled this vacuum from 1770s-1860s; its decline is the story of the vacuum being filled by four independent sufficient-condition institutions.
% FOUNDING_PROBLEM_CORROBORATION: Confirmed by multiple independent historiographies: legal historians (Friedman on state monopoly), military historians (Skelley on West Point professionalization), economic historians (Fligstein on commercial law), cultural historians (Greenberg on dignity culture), Civil War historians (Faust on death and honor). No single beneficiary group attests all four; the corroboration is the convergence of distinct scholarly traditions on the same overdetermined structure.
narrative_ontology:disappearance_verdict(dueling_disappearance_mechanism__overdetermined_composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(dueling_disappearance_mechanism__overdetermined_composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__overdetermined_composite_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(dueling_disappearance_mechanism__overdetermined_composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dueling_disappearance_mechanism__overdetermined_composite_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dueling_disappearance_mechanism__overdetermined_composite_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dueling_disappearance_mechanism__overdetermined_composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects the constraint's hybrid character: genuine coordination (state violence monopoly, contract enforcement, professional discipline) coexists with asymmetric extraction (planter elite stripped of extrajudicial power, aristocratic officers stripped of honor capital, frontier men stripped of only dispute resolution). Suppression (0.48) is moderate — legal prohibition required active enforcement but cultural shift was largely self-propagating; Civil War was exogenous shock. Theater ratio (0.35) captures performative honor rhetoric persisting after functional substitution (postwar veterans' organizations, Lost Cause mythology). Accessibility collapse (0.72) is high — once the four suppressors operated, alternatives to dueling (courts, military discipline, commercial reputation) became the only viable paths; the honor system's internal logic collapsed. Resistance (0.41) is moderate — planter elite resisted Reconstruction enforcement; frontier populations resisted territorial courts; but no organized counter-movement restored dueling. The tangled_rope classification fits: multiple beneficiaries from different mechanisms, victims whose identity-locked or trapped exit prevents escape, active enforcement required (statutes, courts-martial, Reconstruction occupation).
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (aristocratic officers, planter elite, honor practitioners, frontier men) experience this as snare-like extraction — their dispute-resolution and status mechanisms are criminalized and replaced by systems they did not choose and cannot exit. The agenda-setter/beneficiary seats (state, military, commercial, professional, Reconstruction) experience it as rope-like coordination — genuine collective-action problems solved by substitute institutions. The engine will compute this divergence from the structural data: same constraint, different seats, different effective extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   State legal monopoly and professional military corps are near-beneficiary (d ~ 0.1-0.2): they designed and enforce the substitute systems. Commercial banking and bourgeois professions are beneficiaries (d ~ 0.2-0.3): they gain from impersonal systems but didn't design the suppression. Postwar Reconstruction governments are agenda-setters with constrained exit (d ~ 0.3) — they enforce but depend on federal power. Aristocratic officers are identity-locked payers (d ~ 0.9): their self-concept fuses with dueling; professionalization makes their honor capital worthless. Planter elite are trapped payers (d ~ 0.95): economic base destroyed, enforcement tool criminalized, no exit from collapsing order. Honor culture practitioners are constrained payers (d ~ 0.7): cultural shift devalues their capital but migration carries the culture. Frontier men are trapped payers and excluded (d ~ 0.95): no state protection, violence is survival, excluded from legislative process. Historical sociologist is analytical (d = 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (authority vacuum in post-colonial America) is dead — the four substitute institutions filled it. Yet the cultural rhetoric of honor persists (theater), and the constraint's classification prevents mislabeling: if only coordination were seen, the asymmetric extraction from identity-locked and trapped payers would be missed (false rope). If only extraction were seen, the genuine collective-action solutions (state monopoly, contract law, professional discipline) would be missed (false snare). The tangled_rope classification captures the overdetermined structure: multiple sufficient-condition coordination functions, each with its own extraction profile, operating simultaneously. The mandate (dueling as dispute resolution) atrophied because four independent sufficient alternatives emerged; the constraint's persistence is the substitute institutions' persistence, not dueling's.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_nonseparability,
    'Are the four sufficient conditions (legal, institutional, cultural, trauma) truly non-separable in their effects on dueling''s decline, or does historical analysis reveal a dominant pathway with the others as accelerants?',
    'Counterfactual historical modeling: simulate dueling''s trajectory with each suppressor removed individually and in combination. If any single suppressor''s removal fails to restore dueling above a persistence threshold, non-separability is confirmed.',
    'If separable, ε becomes measurable per pathway and the constraint decomposes into multiple tangled_rope or snare constraints. If non-separable, the composite reading is the only ε-invariant description and the sibling readings are structurally incomplete.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_nonseparability, conceptual, 'Whether the overdetermined structure is analytically irreducible or a descriptive convenience.').

omega_variable(
    victim_set_contingency,
    'Does the victim set depend on which mechanism dominated in a given region/period, making the composite reading''s victim list an aggregate of distinct constraint-victim pairings?',
    'Regional-period analysis: map dueling persistence against local enforcement intensity, cultural indices, military presence, and war mortality. Identify which payer groups correlate with which suppressor''s operation.',
    'If victim-mechanism pairing is stable, the composite reading masks distinct snare/tangled_rope constraints. If fluid, the composite victim set is the correct structural description.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_set_contingency, empirical, 'Whether victims are mechanism-specific or constraint-general.').

omega_variable(
    kernel_reading_relations,
    'What is the structural relationship between this overdetermined_composite_reading and its sibling readings (contraction_reading, institutional_displacement_reading)?',
    'Analyze whether a single historiographical framework could hold both this reading and a sibling reading simultaneously, or whether this reading''s core premise (non-separable overdetermination) logically rules out the sibling''s core premise (single-pathway dominance).',
    'Determines reading_relations in cs_structure: forecloses (mutually exclusive in one framework), coexists_with (different parties hold both), or influences (structural pressure without foreclosure).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_relations, conceptual, 'Structural relationship between this reading and its kernel siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__overdetermined_composite_reading, 1776, 1890).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dueling_overdetermined_tr_t1776, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1776, 0.15).
narrative_ontology:measurement(dueling_overdetermined_tr_t1800, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1800, 0.18).
narrative_ontology:measurement(dueling_overdetermined_tr_t1820, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1820, 0.22).
narrative_ontology:measurement(dueling_overdetermined_tr_t1840, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1840, 0.28).
narrative_ontology:measurement(dueling_overdetermined_tr_t1860, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1860, 0.32).
narrative_ontology:measurement(dueling_overdetermined_tr_t1865, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1865, 0.41).
narrative_ontology:measurement(dueling_overdetermined_tr_t1877, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1877, 0.38).
narrative_ontology:measurement(dueling_overdetermined_tr_t1890, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1890, 0.35).

% Extraction over time
narrative_ontology:measurement(dueling_overdetermined_be_t1776, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1776, 0.35).
narrative_ontology:measurement(dueling_overdetermined_be_t1800, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1800, 0.42).
narrative_ontology:measurement(dueling_overdetermined_be_t1820, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1820, 0.48).
narrative_ontology:measurement(dueling_overdetermined_be_t1840, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1840, 0.55).
narrative_ontology:measurement(dueling_overdetermined_be_t1860, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1860, 0.58).
narrative_ontology:measurement(dueling_overdetermined_be_t1865, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1865, 0.68).
narrative_ontology:measurement(dueling_overdetermined_be_t1877, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1877, 0.65).
narrative_ontology:measurement(dueling_overdetermined_be_t1890, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1890, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(dueling_overdetermined_su_t1776, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1776, 0.25).
narrative_ontology:measurement(dueling_overdetermined_su_t1800, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1800, 0.32).
narrative_ontology:measurement(dueling_overdetermined_su_t1820, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1820, 0.38).
narrative_ontology:measurement(dueling_overdetermined_su_t1840, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1840, 0.45).
narrative_ontology:measurement(dueling_overdetermined_su_t1860, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1860, 0.48).
narrative_ontology:measurement(dueling_overdetermined_su_t1865, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1865, 0.62).
narrative_ontology:measurement(dueling_overdetermined_su_t1877, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1877, 0.52).
narrative_ontology:measurement(dueling_overdetermined_su_t1890, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1890, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dueling_disappearance_mechanism__overdetermined_composite_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__overdetermined_composite_reading, state_violence_monopoly_formation).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__overdetermined_composite_reading, military_professionalization_us).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__overdetermined_composite_reading, commercial_contract_law_19c).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__overdetermined_composite_reading, bourgeois_professional_credentialing).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__overdetermined_composite_reading, reconstruction_state_authority).

% DUAL FORMULATION NOTE:
% Part of dueling_disappearance_mechanism kernel family. This reading (overdetermined_composite_reading) treats causal non-separability as definitive. Sibling contraction_reading treats cultural shift as dominant. Sibling institutional_displacement_reading treats institutional substitution as dominant. All three share the same referent (dueling's disappearance 1776-1890) but author different ε structures and victim/beneficiary mappings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dueling_disappearance_mechanism__overdetermined_composite_reading, organized, 0.85).
constraint_indexing:directionality_override(dueling_disappearance_mechanism__overdetermined_composite_reading, powerful, 0.92).
constraint_indexing:directionality_override(dueling_disappearance_mechanism__overdetermined_composite_reading, moderate, 0.7).
constraint_indexing:directionality_override(dueling_disappearance_mechanism__overdetermined_composite_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
