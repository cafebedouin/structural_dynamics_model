% ============================================================================
% CONSTRAINT STORY: border_legitimacy__humanitarian_obligation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_legitimacy__humanitarian_obligation_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: border_legitimacy__humanitarian_obligation_reading
 *   human_readable: Refugee/Economic Migrant Distinction as Ground of Admission Obligation
 *   domain: political_philosophy/international_law/migration
 *
 * SUMMARY:
 *   This story instantiates the humanitarian_obligation_reading of the
 *   border_legitimacy kernel: states owe admission to those fleeing
 *   persecution or disaster, but not to general economic migrants. The
 *   reading's genuine coordination function is real — a shared, administrable
 *   standard for who gets non-refoulement protection enabled the post-WWII
 *   refugee protection regime and burden-sharing among states. But the same
 *   categorical line that makes the obligation administrable also produces a
 *   bifurcated victim set: recognized refugees benefit from a strong legal
 *   claim, while economic migrants and non-qualifying disaster-displaced
 *   persons — often facing comparably severe conditions — are categorically
 *   excluded from the obligation this reading recognizes. The extraction is
 *   moderate (0.52) because it operates through categorical exclusion rather
 *   than direct transfer: the harm is the withheld obligation, administered
 *   by asylum bureaucracies whose institutional survival depends on the line
 *   remaining stable and litigable.
 *
 * KEY AGENTS:
 *   - recognized_refugees: primary beneficiary of the categorical obligation (powerless/trapped) — protected but status-dependent
 *   - economic_migrants: primary victim of the categorical exclusion (powerless/trapped) — bears the cost of the line without recourse
 *   - climate_displaced_non_refugees: secondary victim — falls into the gap between 'disaster' language and operationalized legal categories
 *   - asylum_adjudication_bureaucracies: institutional agenda-setter administering the line case by case
 *   - receiving_state_publics: beneficiary of a morally legitimating framework that bounds admission numbers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_legitimacy__humanitarian_obligation_reading, 0.52).
domain_priors:suppression_score(border_legitimacy__humanitarian_obligation_reading, 0.61).
domain_priors:theater_ratio(border_legitimacy__humanitarian_obligation_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_legitimacy__humanitarian_obligation_reading, tangled_rope).
narrative_ontology:human_readable(border_legitimacy__humanitarian_obligation_reading, "Refugee/Economic Migrant Distinction as Ground of Admission Obligation").
narrative_ontology:topic_domain(border_legitimacy__humanitarian_obligation_reading, "political_philosophy/international_law/migration").

domain_priors:requires_active_enforcement(border_legitimacy__humanitarian_obligation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_legitimacy__humanitarian_obligation_reading, '5c5dd918-f68a-4a22-8c0c-70737b69309f').
narrative_ontology:cs_kernel_codification('5c5dd918-f68a-4a22-8c0c-70737b69309f', fixed_text).
narrative_ontology:cs_authority_grounding('5c5dd918-f68a-4a22-8c0c-70737b69309f', lineage).
narrative_ontology:cs_interpretation_layer_present('5c5dd918-f68a-4a22-8c0c-70737b69309f').
narrative_ontology:cs_reading_relation('5c5dd918-f68a-4a22-8c0c-70737b69309f', border_legitimacy__sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('5c5dd918-f68a-4a22-8c0c-70737b69309f', border_legitimacy__freedom_of_movement_reading, coexists_with).
narrative_ontology:cs_axiom('5c5dd918-f68a-4a22-8c0c-70737b69309f', foundational, persecution_generates_special_obligation).
narrative_ontology:cs_axiom_status(persecution_generates_special_obligation, holdable).
narrative_ontology:cs_axiom_grounding('5c5dd918-f68a-4a22-8c0c-70737b69309f', persecution_generates_special_obligation, deontological).
narrative_ontology:cs_axiom('5c5dd918-f68a-4a22-8c0c-70737b69309f', foundational, economic_hardship_does_not_ground_admission_claim).
narrative_ontology:cs_axiom_status(economic_hardship_does_not_ground_admission_claim, holdable).
narrative_ontology:cs_axiom_grounding('5c5dd918-f68a-4a22-8c0c-70737b69309f', economic_hardship_does_not_ground_admission_claim, conventional).
narrative_ontology:cs_reference_frame('5c5dd918-f68a-4a22-8c0c-70737b69309f', convention_persecution_standard).
narrative_ontology:cs_drift_state('5c5dd918-f68a-4a22-8c0c-70737b69309f', contemporary_mixed_migration_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('5c5dd918-f68a-4a22-8c0c-70737b69309f', '').
narrative_ontology:cs_kernel_id(border_legitimacy__humanitarian_obligation_reading, border_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_legitimacy__humanitarian_obligation_reading, recognized_refugees).
narrative_ontology:constraint_beneficiary(border_legitimacy__humanitarian_obligation_reading, receiving_state_publics).
narrative_ontology:constraint_beneficiary(border_legitimacy__humanitarian_obligation_reading, asylum_adjudication_bureaucracies).
narrative_ontology:constraint_victim(border_legitimacy__humanitarian_obligation_reading, economic_migrants).
narrative_ontology:constraint_victim(border_legitimacy__humanitarian_obligation_reading, climate_displaced_non_refugees).
narrative_ontology:constraint_victim(border_legitimacy__humanitarian_obligation_reading, asylum_seekers_with_weak_documentation).
narrative_ontology:constraint_vindicates(border_legitimacy__humanitarian_obligation_reading, persecution_based_admission_standard).
narrative_ontology:constraint_vindicates(border_legitimacy__humanitarian_obligation_reading, non_refoulement_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Have fled persecution meeting the 1951 Convention's grounds (race, religion, nationality, political opinion, particular social group) and, once so categorized, receive a legal claim to admission and non-refoulement that economic migrants lack. Their entire legal existence in the receiving state depends on successfully proving they fit this category rather than the excluded one.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, recognized_refugees, beneficiary,
    powerless, biographical, trapped, national).

% Flee conditions — extreme poverty, state collapse, gang violence not framed as political persecution, wage destitution — that are often as life-threatening as persecution but do not fit the Convention categories. They are categorically denied the admission obligation this reading recognizes, deported or barred with no comparable claim, regardless of the severity of what they are fleeing.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, economic_migrants, payer,
    powerless, biographical, trapped, global).

% Displaced by disasters, sea-level rise, or ecological collapse that this reading's own language ('disaster') gestures toward covering, but which existing legal instruments interpreting the reading rarely recognize as grounds for a binding admission claim. They fall into a gap the reading names but does not operationalize.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, climate_displaced_non_refugees, payer,
    powerless, generational, trapped, global).

% May have genuine persecution claims but lack the documentation, legal representation, or narrative coherence that adjudication systems demand to sort them into the protected category. The distinction this reading draws is administered by others; misclassification here means treatment as an excluded economic migrant regardless of the underlying facts.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, asylum_seekers_with_weak_documentation, payer,
    powerless, biographical, trapped, national).

% Get a moral framework that permits admitting a bounded, categorically limited population while retaining broad discretion to exclude the much larger population of economic migrants. This lets states claim humanitarian legitimacy without accepting open admission, managing both international reputation and domestic political pressure over migration numbers.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, receiving_state_publics, beneficiary,
    organized, generational, mobile, national).

% Administer the persecution/economic-migrant line case by case, developing the doctrine, credibility standards, and country-of-origin assessments that operationalize the distinction. They control which narratives count as persecution and which count as 'merely' economic, and their institutional survival depends on the distinction remaining administrable and legally stable.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, asylum_adjudication_bureaucracies, agenda_setter,
    institutional, generational, arbitrage, national).

% Argue the persecution/economic distinction is morally arbitrary — that destitution and state violence produce comparably forced movement — and press for broader protection categories including climate and generalized-violence displacement. They participate in litigation and advocacy but do not set the adjudicative categories themselves.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, human_rights_advocacy_organizations, excluded,
    organized, generational, constrained, global).

% Often benefit from emigration (remittances, reduced domestic pressure) regardless of whether departures are classified as persecution-driven or economic, but have no voice in how receiving states draw the distinction that determines whether their departing citizens are admitted or returned.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, sending_state_governments, excluded,
    moderate, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_legitimacy__humanitarian_obligation_reading, diffuse).
narrative_ontology:fixing_cost_class(border_legitimacy__humanitarian_obligation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a workable, internationally shared standard (rooted in the 1951 Refugee Convention) for identifying a bounded class of people whom states commit to admitting and not returning to danger, allowing multilateral cooperation on burden-sharing and preventing races to the bottom in protection standards.
% TRANSFER_FUNCTION: Moves admission, legal status, and protection from refoulement toward those classified as persecution-fleeing, while withholding the same from those classified as economic migrants or non-qualifying disaster-displaced — the transfer is categorical legal status, not resources directly, though status determines access to labor markets, housing, and safety.
% ABSENT_VOICES: Economic migrants and climate-displaced non-refugees have no forum in which to contest the persecution/economic line itself; they can only try to fit their claims into the existing category. Sending-state governments and destination-country labor markets that might benefit from more open economic migration are not parties to the adjudicative process that draws the line.
% DISAPPEARANCE_RATIONALE: If the persecution/economic-migrant distinction vanished overnight, either all forced movement would require justification on a single undifferentiated standard, or states would lose the specific moral vocabulary currently used to justify selective admission — asylum bureaucracies, refugee status determination systems, and international protection instruments would need to be rebuilt on a different basis, and millions currently in refugee status limbo or economic-migrant exclusion would face reclassification.
% FOUNDING_PROBLEM: After WWII, states needed a way to commit to not returning people to political persecution and genocide-adjacent danger without accepting unlimited obligations to admit anyone experiencing hardship — the 1951 Convention drew a line intended to make protection commitments politically sustainable and administrable.
% FOUNDING_PROBLEM_CORROBORATION: UNHCR and refugee law scholars attest the persecution-based standard remains a live, functioning international commitment addressing ongoing political violence. Migration scholars, climate displacement researchers, and organizations like the Global Compact for Migration secretariat attest — from outside the beneficiary set of adjudication bureaucracies and receiving-state governments — that the category increasingly fails to track who is actually fleeing life-threatening conditions, particularly for climate and generalized-violence displacement, suggesting the founding problem has partially mutated beyond what the category was built to solve.
narrative_ontology:disappearance_verdict(border_legitimacy__humanitarian_obligation_reading, world_rearranges).
narrative_ontology:founding_problem_status(border_legitimacy__humanitarian_obligation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_legitimacy__humanitarian_obligation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(border_legitimacy__humanitarian_obligation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(border_legitimacy__humanitarian_obligation_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_legitimacy__humanitarian_obligation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_legitimacy__humanitarian_obligation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_legitimacy__humanitarian_obligation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.52) reflects that roughly half the effective burden of this reading falls on those it excludes rather than includes — the categorical logic itself, not merely enforcement, produces the harm. Suppression (0.61) is substantial because maintaining the line requires active border enforcement, detention, and deportation infrastructure against those classified outside it; this has intensified over the measured interval (0.40 to 0.61) as adjudication systems have hardened credibility standards and expanded detention capacity in response to rising claim volumes. Theater ratio rose from 0.15 to 0.40, reflecting the growing gap between the reading's protective rhetoric ('fleeing persecution or disaster') and the narrowly operationalized categories actually applied — 'disaster' in particular functions more as aspirational language than as a working legal standard for most claimants displaced by climate or generalized violence.
 *
 * PERSPECTIVAL GAP:
 *   From the refugee-advocacy seat, this reading looks like a genuine, hard-won moral achievement — a real coordination solution that ended eras of unconditional refoulement. From the economic-migrant seat, the identical categorical structure looks like an arbitrary and often lethal exclusion dressed in humanitarian language, since the underlying severity of what people flee frequently exceeds the legal category that would protect them. The engine's per-seat computation should register both readings as structurally coherent responses to the same base data, not as one correct and one mistaken.
 *
 * DIRECTIONALITY LOGIC:
 *   Recognized refugees sit near the beneficiary end: the reading's central legal architecture exists to protect them, though their status is precarious and contingent on successful classification. Economic migrants and climate-displaced non-refugees sit near the full-target end: the reading's core distinguishing move is to withhold from them exactly what it grants to refugees, and their exit options are trapped by the same global economic and ecological forces that displaced them. Asylum adjudication bureaucracies function as agenda-setters with institutional exit (arbitrage) — they can adjust doctrine and standards without bearing the consequences of misclassification. Receiving-state publics benefit from bounded admission obligations that let them claim humanitarian legitimacy without open borders.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing refoulement of those fleeing political persecution — remains partly live: political persecution has not disappeared. But the reading's operational boundary has not kept pace with how forced displacement actually occurs today (climate, generalized gang and cartel violence, state collapse without classic political persecution), producing a mandatrophy-adjacent condition where the category persists in a form increasingly decoupled from the full range of humanitarian emergencies it was rhetorically built to address. This is not full mandatrophy (the persecution-based claim is still substantively enforced for those who fit it) but a documented drift consistent with founding_problem_status: contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    persecution_economic_line_naturalness,
    'Is the persecution/economic-migrant distinction a defensible moral line tracking a real difference in state responsibility, or a constructed administrative convenience that happens to track which claims are cheapest for receiving states to deny?',
    'Comparative analysis of severity outcomes (mortality, violence exposure, life prospects) between recognized refugees and excluded economic migrants/climate-displaced persons who were denied protection; if outcome severity is comparable across the line, the distinction''s moral grounding weakens relative to its administrative convenience.',
    'If the line tracks administrative convenience more than moral desert, the reading functions closer to a snare for the excluded population wearing tangled_rope''s coordination language; if it tracks a genuine moral distinction (state agency in persecution vs. diffuse economic causation), the tangled_rope classification is more defensible as capturing real, if imperfect, coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(persecution_economic_line_naturalness, conceptual, 'Whether the refugee/economic-migrant line is morally principled or administratively convenient.').

omega_variable(
    kernel_reading_selection_evidence,
    'What in the source material or context selects the humanitarian_obligation_reading as the operative reading for this analysis, rather than sovereignty_reading (no obligation at all) or freedom_of_movement_reading (obligation to all, not just the persecuted)?',
    'Trace which reading is embedded in the dominant international legal instruments currently in force (1951 Convention, its 1967 Protocol) versus which readings are advocated by which political and advocacy coalitions; the currently codified international law leans toward this reading, which is why it was selected as the manifest''s anchor reading rather than an average of the three.',
    'If sovereignty_reading were instead treated as the operative kernel reading, victims would include all denied claimants without the refugee/economic distinction mattering, and ε would likely be lower (exclusion treated as a right, not a violated obligation) with a different beneficiary/victim topology. If freedom_of_movement_reading were operative, the same border enforcement apparatus would appear far more extractive with a much larger victim set (all excluded migrants, refugee or not) and higher ε.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_selection_evidence, conceptual, 'Why this specific reading was instantiated among the three declared kernel readings.').

omega_variable(
    disaster_language_operationalization_gap,
    'Does ''disaster'' in the reading''s own framing genuinely extend the protected category to climate and generalized-catastrophe displacement, or is it rhetorical scope that has never been operationalized in binding law?',
    'Survey of state practice and case law testing whether any jurisdiction has granted binding non-refoulement protection on disaster grounds absent an underlying persecution nexus; track the trajectory of proposals (e.g., Nansen Initiative, Platform on Disaster Displacement) toward or away from binding status.',
    'If ''disaster'' remains purely rhetorical, the theater_ratio trajectory understates the gap between claimed and operative scope, and climate_displaced_non_refugees'' payer status is even more structurally locked-in than the current metrics reflect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disaster_language_operationalization_gap, empirical, 'Whether disaster-based admission is a real or merely rhetorical component of this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_legitimacy__humanitarian_obligation_reading, 1951, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1951, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 1951, 0.15).
narrative_ontology:measurement(bord_tr_t1970, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(bord_tr_t1990, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 1990, 0.27).
narrative_ontology:measurement(bord_tr_t2005, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 2005, 0.32).
narrative_ontology:measurement(bord_tr_t2015, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 2015, 0.37).
narrative_ontology:measurement(bord_tr_t2024, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(bord_be_t1951, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 1951, 0.32).
narrative_ontology:measurement(bord_be_t1970, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 1970, 0.38).
narrative_ontology:measurement(bord_be_t1990, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 1990, 0.44).
narrative_ontology:measurement(bord_be_t2005, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 2005, 0.48).
narrative_ontology:measurement(bord_be_t2015, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 2015, 0.5).
narrative_ontology:measurement(bord_be_t2024, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 2024, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1951, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 1951, 0.4).
narrative_ontology:measurement(bord_su_t1970, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 1970, 0.46).
narrative_ontology:measurement(bord_su_t1990, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 1990, 0.52).
narrative_ontology:measurement(bord_su_t2005, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 2005, 0.56).
narrative_ontology:measurement(bord_su_t2015, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 2015, 0.59).
narrative_ontology:measurement(bord_su_t2024, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 2024, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_legitimacy__humanitarian_obligation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(border_legitimacy__humanitarian_obligation_reading, 0.1).
narrative_ontology:affects_constraint(border_legitimacy__humanitarian_obligation_reading, border_legitimacy__sovereignty_reading).
narrative_ontology:affects_constraint(border_legitimacy__humanitarian_obligation_reading, border_legitimacy__freedom_of_movement_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the border_legitimacy kernel. sovereignty_reading treats exclusion as an unqualified right (lower ε, no obligation-based victim set); freedom_of_movement_reading treats all border exclusion as presumptively illegitimate (much higher ε, universal victim set). This reading occupies the contested middle: a real but categorically bounded obligation whose line-drawing itself produces a bifurcated victim set. Each reading is authored as a separate constraint with its own stable ε per the ε-invariance principle; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
