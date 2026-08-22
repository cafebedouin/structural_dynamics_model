% ============================================================================
% CONSTRAINT STORY: total_war_winnability_post1945__strategic_culture_drift
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_winnability_post1945__strategic_culture_drift, []).

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
 *   constraint_id: total_war_winnability_post1945__strategic_culture_drift
 *   human_readable: Strategic-Culture Drift Exclusion of Total War from Elite Discourse
 *   domain: international_relations/strategic_studies/commitment_systems
 *
 * SUMMARY:
 *   Between 1945 and the present, total war - the pursuit of an enemy
 *   society's complete defeat through full national mobilization - migrated
 *   from the center of elite strategic discourse to its margins without ever
 *   leaving the materially reachable space. This story authors the
 *   strategic_culture_drift reading of the contested kernel
 *   total_war_winnability_post1945: the binding force examined here is
 *   neither law (the normative reading's candidate, authored separately) nor
 *   nuclear physics (the structural reading's candidate, authored separately)
 *   but the ideational economy of the strategic profession itself. The
 *   standing arrangement under assessment - the epsilon referent - is the
 *   discursive regime that routes attention, curricula, and careers around
 *   the abandoned branch of the option tree, assessed by this reading's own
 *   lights; the repaired state this reading would endorse (full-spectrum
 *   deliberation) is not the referent. The claim/metric gap is deliberate:
 *   the constraint is CLAIMED as piton (an atrophied exclusion persisting on
 *   institutional inertia) while the metrics describe its actual operation
 *   independently; the engine computes per-seat classifications from the
 *   structural data. KEY AGENTS (by structural relationship): -
 *   war_college_faculties_journal_editors: agenda setter
 *   (institutional/constrained) - administers curricula, peer review, and
 *   canon - limited_war_defense_intellectuals: primary beneficiary
 *   (organized/identity_locked) - paradigm holders gaining the vacated
 *   discursive share - great_power_defense_planning_establishments: primary
 *   target (institutional/constrained) - bears the atrophied option-range -
 *   crisis_decision_makers: secondary target (powerful/trapped) - decides at
 *   upper thresholds with narrowed frames - total_war_revisionist_analysts:
 *   excluded voice (moderate/mobile) - democratic_publics: excluded voice
 *   (moderate/trapped) - strategic_studies_meta_analysts: analytical observer
 *   (analytical/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_winnability_post1945__strategic_culture_drift, 0.5).
domain_priors:suppression_score(total_war_winnability_post1945__strategic_culture_drift, 0.15).
domain_priors:theater_ratio(total_war_winnability_post1945__strategic_culture_drift, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, extractiveness, 0.5).
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_winnability_post1945__strategic_culture_drift, piton).
narrative_ontology:human_readable(total_war_winnability_post1945__strategic_culture_drift, "Strategic-Culture Drift Exclusion of Total War from Elite Discourse").
narrative_ontology:topic_domain(total_war_winnability_post1945__strategic_culture_drift, "international_relations/strategic_studies/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_winnability_post1945__strategic_culture_drift, 'c3f0ce22-6694-4274-a848-567deb596569').
narrative_ontology:cs_kernel_codification('c3f0ce22-6694-4274-a848-567deb596569', distributed).
narrative_ontology:cs_authority_grounding('c3f0ce22-6694-4274-a848-567deb596569', distributed).
narrative_ontology:cs_reading_relation('c3f0ce22-6694-4274-a848-567deb596569', total_war_winnability_post1945__normative_reading_drop, coexists_with).
narrative_ontology:cs_reading_relation('c3f0ce22-6694-4274-a848-567deb596569', total_war_winnability_post1945__structural_contraction_reading, forecloses).
narrative_ontology:cs_axiom('c3f0ce22-6694-4274-a848-567deb596569', foundational, total_war_remains_materially_reachable).
narrative_ontology:cs_axiom_status(total_war_remains_materially_reachable, holdable).
narrative_ontology:cs_axiom_grounding('c3f0ce22-6694-4274-a848-567deb596569', total_war_remains_materially_reachable, empirically_contingent).
narrative_ontology:cs_axiom('c3f0ce22-6694-4274-a848-567deb596569', foundational, option_availability_is_discursively_maintained).
narrative_ontology:cs_axiom_status(option_availability_is_discursively_maintained, holdable).
narrative_ontology:cs_axiom_grounding('c3f0ce22-6694-4274-a848-567deb596569', option_availability_is_discursively_maintained, empirically_contingent).
narrative_ontology:cs_reference_frame('c3f0ce22-6694-4274-a848-567deb596569', total_war_live_elite_option).
narrative_ontology:cs_drift_state('c3f0ce22-6694-4274-a848-567deb596569', contemporary_multipolar_nuclear_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c3f0ce22-6694-4274-a848-567deb596569', '').
narrative_ontology:cs_kernel_id(total_war_winnability_post1945__strategic_culture_drift, total_war_winnability_post1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__strategic_culture_drift, limited_war_defense_intellectuals).
narrative_ontology:constraint_victim(total_war_winnability_post1945__strategic_culture_drift, great_power_defense_planning_establishments).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__strategic_culture_drift, war_college_faculties_journal_editors).
narrative_ontology:constraint_victim(total_war_winnability_post1945__strategic_culture_drift, crisis_decision_makers).
narrative_ontology:constraint_vindicates(total_war_winnability_post1945__strategic_culture_drift, nuclear_revolution_thesis).
narrative_ontology:constraint_vindicates(total_war_winnability_post1945__strategic_culture_drift, limited_war_paradigm_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build careers around deterrence theory, graduated escalation, counterinsurgency, and alliance management. The professional conversation they inhabit has, since the 1950s, allocated its journals, syllabus slots, and funding lines to these frameworks; analysis of society-wide war occupies almost none of it. Their standing rests on the continued centrality of limited-war questions. Few actively defend the boundary, but none has an interest in seeing the abandoned territory reoccupied, and leaving the framework would mean repudiating the body of work their reputations rest on.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, limited_war_defense_intellectuals, beneficiary,
    organized, biographical, identity_locked, global).

% Teach at staff and war colleges, edit the field's journals, and set canonical reading lists. Each cohort inherits curricula in which society-wide war appears as history (1939-1945) or as deterrence backdrop, never as a live planning option. Admitting such analysis as a current subject would require redesigning courses, inviting political misreadings of what the institution prepares for, and defending the change before boards that reward continuity. They could change the discourse within a budget cycle; the inherited syllabus makes each year's default the easiest path.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, war_college_faculties_journal_editors, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(total_war_winnability_post1945__strategic_culture_drift, war_college_faculties_journal_editors, beneficiary).

% Run the defense planning cycles of major powers. The option-range their doctrine covers has narrowed over decades: mass mobilization of society, industrial conversion, and the pursuit of enemy capitulation are studied as history rather than prepared as plans. When crises brush upper thresholds, as in 2022, staffs discover the analytical scaffolding for the highest rungs must be improvised under time pressure. They cannot leave their planning function, and reviving the discarded branch internally collides with the same inherited defaults.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, great_power_defense_planning_establishments, payer,
    institutional, generational, constrained, continental).

% Heads of government, ministers, and national security principals who must decide in real time near the upper thresholds of conflict. The frames available to them come from the advice ecosystem above; where that ecosystem no longer rehearses the highest rungs, they confront the steepest choices with the thinnest preparation. They cannot exit the decision moment, and the gap becomes visible only when it is too late to close it.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, crisis_decision_makers, payer,
    powerful, immediate, trapped, national).

% Historians of industrial mobilization, nuclear-employment analysts, and a smaller band of escalation theorists who argue the abandoned branch of the option tree deserves live study. Their submissions to mainstream security journals are routinely reframed as history or deterrence commentary; they publish instead in history venues, niche policy outlets, or open-source channels aimed at audiences abroad. Exit is easy; the cost is relevance inside the conversation they wanted to join.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, total_war_revisionist_analysts, excluded,
    moderate, biographical, mobile, global).

% Would have to consent, through elections, legislatures, and taxation, to any material revival of society-wide war preparation. They are absent from the elite conversation that dropped the topic, and they cannot opt out of the consequences of the resulting unpreparedness. Their attention is episodic, summoned by commemoration rather than deliberation.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, democratic_publics, excluded,
    moderate, generational, trapped, national).

% Historians and sociologists of the strategic studies field who map how its agenda moved. They take testimony from all the other positions, track citation and syllabus data across decades, and owe allegiance to none of the paradigms in contention.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, strategic_studies_meta_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_winnability_post1945__strategic_culture_drift, diffuse).
narrative_ontology:fixing_cost_class(total_war_winnability_post1945__strategic_culture_drift, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates scarce elite strategic attention: by dropping society-wide war from the agenda, the professional community concentrates analytic bandwidth, curricula, and wargame design on conflict modes judged plausible (deterrence, limited war, proxy and hybrid conflict), and spares civil-military relations the recurring political strain of publicly preparing for societal annihilation.
% TRANSFER_FUNCTION: Moves discursive space, publication slots, curricular hours, research funding, and career validation from analysis of total war toward limited-war frameworks; moves conceptual preparedness for upper-threshold conflict out of the planning system without transferring it to any seat - the stock is dissipated, not collected.
% ABSENT_VOICES: Mobilization historians and logisticians who know what society-wide war actually requires; adversary planners (Russian, Chinese) who continue gaming escalatory totality in open doctrine while Western venues decline the subject; and democratic publics whose consent any material revival would require. All stand outside the curated conversation.
% DISAPPEARANCE_RATIONALE: If the exclusion lifted overnight - total war returning to syllabi, journals, and wargames as a routinely analyzed option - curricula and canon lists would be rewritten, limited-war paradigm dominance would break, planning establishments would rebuild mobilization and employment doctrine they currently lack, and civil-military politics would reopen questions closed since 1945. Careers, institutions, and the field's self-understanding are arranged around the absence.
% FOUNDING_PROBLEM: After 1945 the problem was to make limited war thinkable and choosable: to stop total war - the default logic of the wars just fought - from swallowing all planning below the apocalyptic ceiling, and to keep elite discourse from normalizing civilization-ending war as a routine instrument.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the beneficiary set by military historians of industrial mobilization documenting the deliberate post-1945 abandonment of total-war planning; by former senior officials' accounts of discovering, during the 2022 Ukraine crisis, how thin nuclear-employment planning had become; and by open-source adversary doctrine showing rival establishments never stopped gaming totality. No voice inside the limited-war beneficiary set claims the exclusion still performs its founding function.
narrative_ontology:disappearance_verdict(total_war_winnability_post1945__strategic_culture_drift, world_rearranges).
narrative_ontology:founding_problem_status(total_war_winnability_post1945__strategic_culture_drift, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_winnability_post1945__strategic_culture_drift, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(total_war_winnability_post1945__strategic_culture_drift, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_winnability_post1945__strategic_culture_drift, 0.5, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_winnability_post1945__strategic_culture_drift_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(total_war_winnability_post1945__strategic_culture_drift, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(total_war_winnability_post1945__strategic_culture_drift_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.50: the arrangement imposes a real but diffuse loss - narrowed option-range, improvised crisis planning at upper thresholds - that lies latent most years and bites episodically. Suppression is low (0.15) because the exclusion no longer runs on coercive machinery: the loyalty-era pressures of the late 1940s decayed into editorial norm and self-censorship, which is why the suppression_requirement series is authored as an enforcement-decay trajectory - this story specifically traces enforcement capacity eroding from 0.55 to 0.15, a legitimate use of the series rather than a restatement of the static scalar. Theater_ratio is high (0.65) and rising: commemoration, ritualized invocation, and wargames that assume away the upper branches now constitute most activity referencing the exclusion, while functional maintenance approaches zero - nobody polices the boundary, everyone inherits it. Accessibility_collapse (0.38) is partial: understanding the drift reveals that reviving the discourse is available but costly, unlike a natural limit where alternatives collapse completely. Resistance (0.30) is episodic revisionism - mobilization historians and post-2022 escalation analysts - insufficient to reverse the drift. All three series share one nine-point grid (1945-2025 at decade steps) so the engine samples aligned rows; the late rise in base_extractiveness (0.44 to 0.50) models the accumulating cost of atrophy as great-power competition returns, which temporal analysis may flag for investigation. The dynamics are monotonic drift, not oscillation, so no cyclical machinery is invoked. Receipt surface: the extracted good - conceptual preparedness for upper-threshold conflict - is dissipated rather than delivered; the beneficiary seat gains relatively (the vacated share of attention and funding) but receives no transferred stock, so gain_flow is authored as diffuse after checking every named seat. Fixing cost is prohibitive relative to benefit: reconstruction demands curriculum redesign, reputational exposure, and political explanation against a benefit that is speculative and deferred.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently because the arrangement's incidence is asymmetric. From the agenda-setting seat (faculties, editors), the exclusion is simply the inherited shape of a respectable field - change is possible but seems pointless, and the seat experiences near-zero imposition. From the payer seats (planning establishments, crisis deciders), the same arrangement is a missing wing of the building: unfelt until a fire reaches it, catastrophic then. The beneficiary seat experiences validation, not imposition. The excluded seats (revisionists, publics) perceive gatekeeping that insiders do not experience as gatekeeping at all. The engine derives these divergent classifications from power, exit, and directional position; the divergence between the agenda-setter's experienced benignity and the payer's latent exposure is the perspectival fact this story encodes.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation. limited_war_defense_intellectuals declare as beneficiaries with identity_locked exit: their professional selves are constituted by the frameworks the vacated space protects, so their directionality sits near the beneficiary pole and their position is stable against paradigm turnover. great_power_defense_planning_establishments and crisis_decision_makers declare as victims - constrained and trapped respectively - placing them near the target pole, with the trapped deciders nearest full-target exposure since no exit from the decision moment exists. The agenda-setter seat is dual-positioned (administers and benefits) and derives a low-to-symmetric d. Continental and national scopes scale verification difficulty modestly upward for the payer seats: atrophied preparedness is hard to audit until exercised. No directionality overrides are authored - the structural derivation captures every seat's relationship. Suppression is authored as a raw structural property and is not scaled; only extractiveness is scaled by directionality and scope in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - keeping limited war choosable against a default-totalizing military logic - died with the drift it was meant to manage: the danger reversed, and nothing now threatens to swallow limited options from above. The arrangement persists anyway, carried by syllabi, canon lists, and the absence of any actor hurt enough annually to sponsor reconstruction. Classifying this as a rope (benign attention allocation) would miss the real, if latent, injury to planning capacity; classifying it as a snare (deliberate gatekeeping) would overcredit a maintenance effort that no longer exists - no seat works to keep total war out. The piton reading holds both truths: atrophied function, inertial persistence, diffuse cost, and a beneficiary class that gains relatively without maintaining anything. The R5 interview records the mismatch directly: founding_problem_status dead combined with disappearance_verdict world_rearranges - the world would rearrange, which is precisely the zombie signature the piton category exists to name. Mandatrophy is resolved in the sense that the mandate is spent; the arrangement has not yet noticed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_binding_force_underdetermination,
    'Which force binds total war out of the post-1945 option space - legal-normative prohibition, physical-nuclear removal, or strategic-cultural drift?',
    'Comparative adjudication across the three linked family stories: test whether lifting norms restores practice (normative reading), whether capability exists independent of discourse (structural reading), and whether discourse tracks capability or leads it (this reading).',
    'If the structural reading is correct, this constraint describes a phantom - nothing was forgotten because nothing remained reachable - and classification collapses toward mountain. If the normative reading is correct, the exclusion is law-backed and enforcement-bearing, shifting toward tangled_rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_binding_force_underdetermination, conceptual, 'This story is one reading of the total_war_winnability_post1945 kernel; sibling readings relocate the binding force and would restructure the constraint.').

omega_variable(
    passive_benefit_vs_active_capture,
    'Do limited-war intellectuals actively maintain the exclusion through editorial gatekeeping and curricular policing, or merely fail to disturb an inherited absence?',
    'Trace editorial rejection records, syllabus genealogies, and funding decisions for active filtering versus simple continuity across gatekeeper generations.',
    'Active capture converts the piton reading toward snare or tangled_rope with concentrated gain receipt; passive inheritance supports the piton reading with diffuse incidence and no maintainer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(passive_benefit_vs_active_capture, empirical, 'Whether the beneficiary seat maintains the arrangement or merely inhabits it.').

omega_variable(
    discourse_capacity_coupling,
    'Does discursive atrophy track real material decay - mobilization industry, conscription machinery, stockpile depth - such that reachability itself is eroding?',
    'Industrial-base audits, mobilization-capacity studies, and the measured gap between declared doctrine and executable plans.',
    'If material capacity has decayed irreversibly, this reading converges on the structural reading and the constraint hardens toward mountain; if capacity persists dormant, the piton reading stands and revival remains cheap in principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discourse_capacity_coupling, empirical, 'Whether the reading''s core premise (continued reachability) survives contact with material-capacity evidence.').

omega_variable(
    internalized_vs_structural_exclusion,
    'Is the remaining exclusion maintained by external gatekeeping or by internalized self-censorship among strategically literate elites?',
    'Compare submission and publication patterns before and after gatekeeper turnover; test willingness to produce total-war analyses under conditions removing reputational exposure.',
    'If internalized, the structural suppression measure understates the arrangement''s grip - removing editors would not restore discourse, and effective suppression exceeds the authored scalar.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internalized_vs_structural_exclusion, empirical, 'Structural versus internalized mechanism for the residual exclusion.').

omega_variable(
    constructed_vs_emergent_drift,
    'Was the ideational shift an emergent adaptation to changed conditions or a constructed outcome serving identifiable paradigm investors?',
    'Process-trace the drift''s key episodes (the Massive Retaliation debate, flexible response, the post-Cold War budget consolidation) for identifiable agency versus diffuse adaptation.',
    'A constructed origin raises attributed extractiveness and strengthens the beneficiary declaration; an emergent origin supports treating the exclusion as near-natural adaptation, drifting the classification toward mountain-adjacent readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(constructed_vs_emergent_drift, conceptual, 'Naturalness ambiguity of the discursive exclusion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_winnability_post1945__strategic_culture_drift, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 1945, 0.1).
narrative_ontology:measurement_basis(tota_tr_t1945, observed).
narrative_ontology:measurement(tota_tr_t1955, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 1955, 0.2).
narrative_ontology:measurement_basis(tota_tr_t1955, observed).
narrative_ontology:measurement(tota_tr_t1965, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 1965, 0.3).
narrative_ontology:measurement_basis(tota_tr_t1965, observed).
narrative_ontology:measurement(tota_tr_t1975, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 1975, 0.4).
narrative_ontology:measurement_basis(tota_tr_t1975, observed).
narrative_ontology:measurement(tota_tr_t1985, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 1985, 0.5).
narrative_ontology:measurement_basis(tota_tr_t1985, observed).
narrative_ontology:measurement(tota_tr_t1995, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 1995, 0.55).
narrative_ontology:measurement_basis(tota_tr_t1995, observed).
narrative_ontology:measurement(tota_tr_t2005, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 2005, 0.6).
narrative_ontology:measurement_basis(tota_tr_t2005, observed).
narrative_ontology:measurement(tota_tr_t2015, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 2015, 0.62).
narrative_ontology:measurement_basis(tota_tr_t2015, observed).
narrative_ontology:measurement(tota_tr_t2025, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 2025, 0.65).
narrative_ontology:measurement_basis(tota_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 1945, 0.18).
narrative_ontology:measurement_basis(tota_be_t1945, observed).
narrative_ontology:measurement(tota_be_t1955, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 1955, 0.3).
narrative_ontology:measurement_basis(tota_be_t1955, observed).
narrative_ontology:measurement(tota_be_t1965, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 1965, 0.42).
narrative_ontology:measurement_basis(tota_be_t1965, observed).
narrative_ontology:measurement(tota_be_t1975, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 1975, 0.5).
narrative_ontology:measurement_basis(tota_be_t1975, observed).
narrative_ontology:measurement(tota_be_t1985, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 1985, 0.52).
narrative_ontology:measurement_basis(tota_be_t1985, observed).
narrative_ontology:measurement(tota_be_t1995, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 1995, 0.48).
narrative_ontology:measurement_basis(tota_be_t1995, observed).
narrative_ontology:measurement(tota_be_t2005, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 2005, 0.44).
narrative_ontology:measurement_basis(tota_be_t2005, observed).
narrative_ontology:measurement(tota_be_t2015, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 2015, 0.46).
narrative_ontology:measurement_basis(tota_be_t2015, observed).
narrative_ontology:measurement(tota_be_t2025, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 2025, 0.5).
narrative_ontology:measurement_basis(tota_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 1945, 0.55).
narrative_ontology:measurement_basis(tota_su_t1945, observed).
narrative_ontology:measurement(tota_su_t1955, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 1955, 0.5).
narrative_ontology:measurement_basis(tota_su_t1955, observed).
narrative_ontology:measurement(tota_su_t1965, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 1965, 0.4).
narrative_ontology:measurement_basis(tota_su_t1965, observed).
narrative_ontology:measurement(tota_su_t1975, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 1975, 0.3).
narrative_ontology:measurement_basis(tota_su_t1975, observed).
narrative_ontology:measurement(tota_su_t1985, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 1985, 0.25).
narrative_ontology:measurement_basis(tota_su_t1985, observed).
narrative_ontology:measurement(tota_su_t1995, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 1995, 0.18).
narrative_ontology:measurement_basis(tota_su_t1995, observed).
narrative_ontology:measurement(tota_su_t2005, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 2005, 0.16).
narrative_ontology:measurement_basis(tota_su_t2005, observed).
narrative_ontology:measurement(tota_su_t2015, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 2015, 0.15).
narrative_ontology:measurement_basis(tota_su_t2015, observed).
narrative_ontology:measurement(tota_su_t2025, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 2025, 0.15).
narrative_ontology:measurement_basis(tota_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_winnability_post1945__strategic_culture_drift, identity_coordination).
narrative_ontology:affects_constraint(total_war_winnability_post1945__strategic_culture_drift, total_war_winnability_post1945__normative_reading_drop).
narrative_ontology:affects_constraint(total_war_winnability_post1945__strategic_culture_drift, total_war_winnability_post1945__structural_contraction_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the kernel total_war_winnability_post1945 per the epsilon-invariance principle: the colloquial label 'total war became unwinnable/unusable after 1945' conflates three structurally distinct constraints. normative_reading_drop authors the legal-normative prohibition regime (Article 2(4), humanitarian law) with its own epsilon; structural_contraction_reading authors the physical unreachability claim (negligible extraction, mountain-adjacent); this story authors the discursive-exclusion arrangement (atrophied, inertial, piton-class). The upstream members typically serve as cited warrant for the downstream cultural account - 'we banned it' and 'we cannot do it' are offered as explanations for 'we no longer discuss it' - so edges run from this story to both siblings for contamination propagation. Each member carries a single stable epsilon over its own referent; no story hedges across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
