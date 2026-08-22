% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__dignity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_kernel__dignity_reading, []).

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
 *   constraint_id: speech_protection_kernel__dignity_reading
 *   human_readable: Dignity-Conditional Speech Protection (Structural Subordination Limit)
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   This story authors ONE reading of the speech_protection_kernel: the
 *   dignity reading, under which speech protection is conditional on the
 *   expression not functioning as structural subordination of target groups.
 *   Under this reading, group harm is cognizable apart from individual harm,
 *   hate speech and group libel fall outside protection, and the guarantee's
 *   scope is indexed to equal-dignity maintenance. The standing arrangement
 *   under contest — the referent for epsilon — is this conditional-protection
 *   regime as administered, assessed by the reading's own lights; the
 *   endorsed alternative arrangements of sibling readings are different
 *   constraints in different files, not hedges inside this one. The
 *   claim/metric gap is deliberate: the regime is CLAIMED as tangled_rope
 *   (genuine coordination carrying real asymmetric costs under active
 *   enforcement) while the metrics are authored independently as
 *   descriptively true — the engine computes per-seat types from the
 *   structural data, and any divergence between claim and computation is the
 *   measurement the corpus exists to take.
 *
 * KEY AGENTS:
 *   - constitutional_courts: Agenda-setting adjudicator (institutional/constrained) — fixes the boundary's location through precedent
 *   - enforcement_tribunals: Secondary agenda-setter and institutional collector (institutional/constrained) — commissions and prosecutors whose jurisdiction grows with the category
 *   - members_of_targeted_groups: Primary beneficiary (powerless/trapped) — receives participatory standing no member could secure alone
 *   - general_discourse_participants: Diffuse beneficiary (organized/mobile) — shares the guarded environment without upkeep costs
 *   - speakers_of_subordinating_expression: Primary payer (moderate/constrained) — loses protection categorically upon classification
 *   - borderline_contentious_speakers: Secondary payer (moderate/constrained) — carries standing classification risk and marginal self-censorship
 *   - intragroup_dissenters: Excluded voice (powerless/trapped) — group members who reject the official dignitary-harm account
 *   - civil_liberties_observers: Analytical observer (powerful/analytical) — documents and contests the boundary's movement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__dignity_reading, 0.58).
domain_priors:suppression_score(speech_protection_kernel__dignity_reading, 0.55).
domain_priors:theater_ratio(speech_protection_kernel__dignity_reading, 0.26).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, theater_ratio, 0.26).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__dignity_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__dignity_reading, "Dignity-Conditional Speech Protection (Structural Subordination Limit)").
narrative_ontology:topic_domain(speech_protection_kernel__dignity_reading, "constitutional/political").

domain_priors:requires_active_enforcement(speech_protection_kernel__dignity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__dignity_reading, '81507398-3b75-4843-937f-ef2d8fcfbbaf').
narrative_ontology:cs_kernel_codification('81507398-3b75-4843-937f-ef2d8fcfbbaf', fixed_text).
narrative_ontology:cs_authority_grounding('81507398-3b75-4843-937f-ef2d8fcfbbaf', lineage).
narrative_ontology:cs_interpretation_layer_present('81507398-3b75-4843-937f-ef2d8fcfbbaf').
narrative_ontology:cs_reading_relation('81507398-3b75-4843-937f-ef2d8fcfbbaf', speech_protection_kernel__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('81507398-3b75-4843-937f-ef2d8fcfbbaf', speech_protection_kernel__harm_threshold_reading, coexists_with).
narrative_ontology:cs_reading_relation('81507398-3b75-4843-937f-ef2d8fcfbbaf', speech_protection_kernel__marketplace_reading, coexists_with).
narrative_ontology:cs_reading_relation('81507398-3b75-4843-937f-ef2d8fcfbbaf', speech_protection_kernel__democratic_participation_reading, influences).
narrative_ontology:cs_axiom('81507398-3b75-4843-937f-ef2d8fcfbbaf', foundational, group_subordination_unprotects_speech).
narrative_ontology:cs_axiom_status(group_subordination_unprotects_speech, holdable).
narrative_ontology:cs_axiom_grounding('81507398-3b75-4843-937f-ef2d8fcfbbaf', group_subordination_unprotects_speech, deontological).
narrative_ontology:cs_axiom('81507398-3b75-4843-937f-ef2d8fcfbbaf', foundational, group_harm_distinct_from_individual_harm).
narrative_ontology:cs_axiom_status(group_harm_distinct_from_individual_harm, holdable).
narrative_ontology:cs_axiom_grounding('81507398-3b75-4843-937f-ef2d8fcfbbaf', group_harm_distinct_from_individual_harm, empirically_contingent).
narrative_ontology:cs_reference_frame('81507398-3b75-4843-937f-ef2d8fcfbbaf', conditional_equal_dignity_framework).
narrative_ontology:cs_drift_state('81507398-3b75-4843-937f-ef2d8fcfbbaf', online_expression_expansion_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('81507398-3b75-4843-937f-ef2d8fcfbbaf', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__dignity_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__dignity_reading, members_of_targeted_groups).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__dignity_reading, general_discourse_participants).
narrative_ontology:constraint_victim(speech_protection_kernel__dignity_reading, speakers_of_subordinating_expression).
narrative_ontology:constraint_victim(speech_protection_kernel__dignity_reading, borderline_contentious_speakers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__dignity_reading, enforcement_tribunals).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__dignity_reading, borderline_contentious_speakers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decide which expressions fall inside the unprotected category and which remain shielded, case by case, under the charter or basic law. Their precedents fix where the boundary sits; they cannot abandon the adjudicative office without abdicating the interpretive role they hold, and reversing settled dignity doctrine requires extraordinary majorities or constitutional amendment.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, constitutional_courts, agenda_setter,
    institutional, generational, constrained, national).

% Human rights commissions, equality bodies, and prosecutors receive complaints, investigate, and bring actions against allegedly subordinating expression. Each widening of the category enlarges their docket, staffing, and jurisdiction; their case-selection practices in turn shape where the boundary sits in daily operation.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, enforcement_tribunals, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_kernel__dignity_reading, enforcement_tribunals, agenda_setter).

% Live inside the speech environment the regime shapes. When dehumanizing address toward their group draws sanction, they gain participatory standing that no member could secure individually; none can opt out of the group membership that exposes them to the address in the first place.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, members_of_targeted_groups, beneficiary,
    powerless, biographical, trapped, national).

% Inhabit a public sphere in which equal-dignity norms are kept by the regime. They contribute nothing to its upkeep beyond ordinary compliance yet share its protective output, and they can disengage from contested forums at will.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, general_discourse_participants, beneficiary,
    organized, biographical, mobile, national).

% Publish or utter expression that adjudicators classify as group-subordinating. They lose the protection every other speaker retains, face fines, damages, or prosecution, and can avoid liability only by reformulating or abandoning the expression — the classification itself is what they contest.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, speakers_of_subordinating_expression, payer,
    moderate, immediate, constrained, national).

% Engage in sharp political, religious, or satirical speech that sits near the boundary. They keep protection in most cases but carry standing uncertainty about whether a given statement will be reclassified, and they hold back marginally more than they would under unconditional protection. They also enjoy the same guarded environment for everything they say away from the line.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, borderline_contentious_speakers, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_kernel__dignity_reading, borderline_contentious_speakers, beneficiary).

% Belong to the protected groups but reject the official account of what harms their dignity — members who reclaim slurs, who need access to derogatory traditions for art or worship, or who experience the guardianship as condescending. Institutions adjudicate their group's dignity without a channel for their dissent, and they cannot step outside the membership the classification speaks for.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, intragroup_dissenters, excluded,
    powerless, biographical, trapped, national).

% Free-expression organizations, comparative constitutional scholars, and supranational monitoring bodies that track how the boundary moves, publish assessments of decisions, and press for narrowing or widening. They hold no docket and bear no liability; their leverage is argument, comparison, and publicity.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, civil_liberties_observers, observer,
    powerful, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_kernel__dignity_reading, diffuse).
narrative_ontology:fixing_cost_class(speech_protection_kernel__dignity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a public discourse environment in which members of historically subordinated groups can participate as equals rather than as objects of demeaning address. It solves a collective-action problem formal speech freedom cannot: where social hierarchy is steep, formally equal expressive rights leave the already-silenced effectively silent, because no individual lawsuit reaches diffuse, cumulative, group-directed injury.
% TRANSFER_FUNCTION: Moves expressive security from speakers whose expression functions as structural subordination (who lose protection and bear sanction risk) toward members of targeted groups (who gain participatory standing), and moves boundary-defining discretion to courts, commissions, and prosecutors.
% ABSENT_VOICES: Intragroup dissenters who reject the official dignitary-harm account — reclamation users, traditionalist artists, members who find the guardianship patronizing — have no procedural seat: the doctrine speaks for their group's dignity through institutions rather than negotiating it with them. Absolutist-leaning speakers who deny the state any competence to define group dignity are present only as litigants losing, not as participants in the standard's design.
% DISAPPEARANCE_RATIONALE: If the conditionality vanished overnight, group-subordinating expression would become fully protected throughout the jurisdiction, enforcement institutions would lose their dockets and staffing rationale, targeted-group participation patterns in public forums would shift measurably, and the line between hostile political invective and actionable subordination would be renegotiated case by case from zero.
% FOUNDING_PROBLEM: Formal expressive freedom coexisting with effective exclusion: twentieth-century societies in which legally free but socially subordinated groups were driven from public discourse by dehumanizing speech that individual-harm analysis could not reach — no specific victim, diffuse and cumulative injury, injury to standing rather than to a named person.
% FOUNDING_PROBLEM_CORROBORATION: Comparative constitutional history corroborates the founding problem from outside the benefiting parties: the drafting record of postwar European instruments and of ECHR Article 10(2) documents the interwar vilification dynamics the drafters sought to prevent; Canadian and German legislative histories cite documented exclusion patterns preceding adoption. Absolutist scholars dispute the remedy and much of the diagnosis, but few dispute the historical reality of the exclusion pattern itself — corroboration for the problem is broad while corroboration for this reading's solution remains contested.
narrative_ontology:disappearance_verdict(speech_protection_kernel__dignity_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__dignity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__dignity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_protection_kernel__dignity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__dignity_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__dignity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_kernel__dignity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_kernel__dignity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58 at interval end) because the regime strips protection categorically from a defined speech class while leaving the rest of the expressive field intact — the cost is concentrated and severe for those classified, diffuse and mild for everyone else. Suppression (0.55) reflects state coercion deployed against a defined category, tempered by persistent alternatives: reformulation, other venues, other jurisdictions, counterspeech. Theater is low-moderate (0.26): the protective function is substantially real — complaints are investigated, orders issue, participation patterns respond — but a growing share of activity is declaratory and symbolic (statements of principle, anniversary prosecutions, high-visibility low-merit cases). Accessibility collapse is low (0.38) because alternatives survive contact with the rule; resistance is substantial (0.62) because absolutist and libertarian opposition is organized, scholarly, and continuous. The temporal series run on one shared grid (t=0,15,30,45,60,75) with all three metrics authored at every point. The suppression_requirement series is authored deliberately: the story traces enforcement-capacity change — commissions built out mid-interval, criminal provisions routinized, online-era monitoring added — not merely shifting extraction. The trajectory is a monotonic ratchet, not a cycle: each widening of the category has been retained, so no oscillation phase needs documenting.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the agenda-setter seat (courts), the arrangement is doctrine under faithful development — each widening a refinement, each prosecution a boundary clarification. From the beneficiary seats, it is standing made possible: an environment they could not buy individually. From the payer seats, the same structure operates as categorical vulnerability — a protection that evaporates precisely when expression turns most serious. The engine computes this divergence from power, exit, and directional data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Members of targeted groups are declared beneficiaries with trapped exit and no power: the derivation places them near the full-beneficiary end (d near 0.0) — the regime subsidizes exactly what they cannot procure alone. General discourse participants are beneficiaries with mobile exit: subsidized but lightly, since they can walk away. Speakers of subordinating expression and borderline contentious speakers are declared victims with constrained exit: the derivation places them near the full-target end. A directionality override is declared for the moderate power atom (d = 0.72) because pure victim derivation would overshoot: even classified speakers retain protection for the whole remainder of their expressive life and share the guarded environment, so their structural relationship sits below full-target. Enforcement tribunals derive low d as beneficiaries — accurately, since jurisdiction and caseload flow to them with each widening. Courts derive low d as agenda-setters; they administer rather than collect.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two symmetrical mislabels. Opponents frame the regime as pure censorship — a snare wearing a dignity costume; proponents frame it as pure protection — a rope with no costs. Tangled_rope keeps both faces legible: the coordination function (equal-participation discourse conditions) is genuine and would not survive voluntary provision, and the extraction (categorical protection loss, classification risk, institutional self-expansion) rides the same structure. On the R5 genealogy: the founding problem remains live, so no mandatrophy is declared — the arrangement has not outlived its function. The tracked risk is mandate creep rather than obsolescence: the measured rise in base_extractiveness and theater_ratio models the category broadening faster than the underlying exclusion problem widens, which is the drift path by which a tangled rope decays toward snare for the payer seats. The mismatch consumer should watch founding_problem_status (live) against the rising extraction series: if status flips to dead while the series keeps climbing, the zombie flag fires.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint instantiates the dignity_reading of speech_protection_kernel; how would the sibling readings (absolutist_reading, harm_threshold_reading, marketplace_reading, democratic_participation_reading) change the structure if they governed instead?',
    'Cross-file comparison of the sibling constraint stories; no resolution is possible or needed within this file — each reading is a separate epsilon-invariant constraint with its own victim set and boundary structure.',
    'Under the absolutist reading the speaker victim classes disappear entirely; under the harm_threshold reading victims exist only where demonstrable individual harm is shown; under the marketplace reading the remedy shifts from restriction to counterspeech; under the democratic_participation reading protection tracks political function rather than dignitary effect. The disagreement is located in the unit of cognizable harm (individual vs. group vs. discourse process) and in who adjudicates it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one reading of a five-reading kernel; sibling deltas change victim sets and adjudication structure.').

omega_variable(
    subordination_boundary_location,
    'Where exactly does the line sit between protected contentious expression and expression that functions as structural subordination?',
    'Systematic comparative analysis of decided cases across dignity jurisdictions (Canada, Germany, ECtHR line): classification rates of contested speech, reversal rates, and the doctrinal tests actually applied.',
    'A wider operative boundary raises effective extraction on the speaker seats and enlarges the victim class; a narrower boundary shrinks both and pulls the computed profile toward rope. The metric values in this story assume the boundary as currently administered.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subordination_boundary_location, empirical, 'Operative width of the unprotected category under the dignity condition.').

omega_variable(
    group_harm_empirical_basis,
    'Does group-directed subordinating speech actually produce the cumulative status-lowering and participation-suppressing effects the doctrine presupposes?',
    'Social-science study of participation, health, and status outcomes in high-vilification versus regulated environments; natural experiments from jurisdictions adopting or repealing the condition.',
    'If the presupposed effects fail to materialize, the empirically contingent axiom (group harm distinct from individual harm) weakens, and this reading drifts structurally toward the harm_threshold sibling. If confirmed, the coordination function is vindicated and the extraction on speakers is the price of a real good.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(group_harm_empirical_basis, empirical, 'Empirical foundation of the group-harm premise distinguishing this reading from its siblings.').

omega_variable(
    paternalism_displacement_question,
    'Does institutional adjudication of group dignity serve the protected groups'' own evaluative standpoint, or does it displace it?',
    'Measure congruence between tribunal dignitary-harm findings and the surveyed preferences of the affected groups'' own members, including dissident members; track which intra-group voices are heard in proceedings.',
    'Systematic divergence would recast the nominal beneficiaries as objects of the arrangement rather than principals, raising effective extraction on the excluded seat and strengthening the case that the regime coordinates institutions'' self-image more than members'' standing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(paternalism_displacement_question, conceptual, 'Whether the guardianship structure protects or displaces the groups'' own voice.').

omega_variable(
    chilling_spillover_extent,
    'How far does protection loss extend beyond core subordination cases into legitimate contentious, religious, or satirical speech?',
    'Before-and-after studies around doctrine changes; survey and submission-rate data from speakers near the boundary; comparison of self-censorship rates across dignity and non-dignity jurisdictions.',
    'Large spillover would push the payer-seat computation toward the snare end for those seats and would date a tangled_rope-to-snare transition risk; negligible spillover confines the costs to the category as drawn and supports the coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chilling_spillover_extent, empirical, 'Width of the shadow the unprotected category casts over adjacent protected speech.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__dignity_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_kernel__dignity_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(spee_tr_t15, speech_protection_kernel__dignity_reading, theater_ratio, 15, 0.13).
narrative_ontology:measurement(spee_tr_t30, speech_protection_kernel__dignity_reading, theater_ratio, 30, 0.16).
narrative_ontology:measurement(spee_tr_t45, speech_protection_kernel__dignity_reading, theater_ratio, 45, 0.19).
narrative_ontology:measurement(spee_tr_t60, speech_protection_kernel__dignity_reading, theater_ratio, 60, 0.23).
narrative_ontology:measurement(spee_tr_t75, speech_protection_kernel__dignity_reading, theater_ratio, 75, 0.26).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_kernel__dignity_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(spee_be_t15, speech_protection_kernel__dignity_reading, base_extractiveness, 15, 0.36).
narrative_ontology:measurement(spee_be_t30, speech_protection_kernel__dignity_reading, base_extractiveness, 30, 0.43).
narrative_ontology:measurement(spee_be_t45, speech_protection_kernel__dignity_reading, base_extractiveness, 45, 0.49).
narrative_ontology:measurement(spee_be_t60, speech_protection_kernel__dignity_reading, base_extractiveness, 60, 0.54).
narrative_ontology:measurement(spee_be_t75, speech_protection_kernel__dignity_reading, base_extractiveness, 75, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_kernel__dignity_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(spee_su_t15, speech_protection_kernel__dignity_reading, suppression_requirement, 15, 0.41).
narrative_ontology:measurement(spee_su_t30, speech_protection_kernel__dignity_reading, suppression_requirement, 30, 0.47).
narrative_ontology:measurement(spee_su_t45, speech_protection_kernel__dignity_reading, suppression_requirement, 45, 0.52).
narrative_ontology:measurement(spee_su_t60, speech_protection_kernel__dignity_reading, suppression_requirement, 60, 0.55).
narrative_ontology:measurement(spee_su_t75, speech_protection_kernel__dignity_reading, suppression_requirement, 75, 0.57).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__dignity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, marketplace_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, democratic_participation_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'free speech protection.' The single natural-language concept covers five structurally distinct claims with different epsilon values, victim sets, and adjudication structures; per the epsilon-invariance principle they are authored as five linked stories rather than one story with a measurement parameter. The absolutist reading is the upstream baseline from which the conditional readings depart; this dignity reading exerts downstream pressure on the democratic_participation sibling by shrinking the protected domain that reading presumes (political expression that subordinates loses its shield), changing that sibling's operating environment without resolving the dispute between them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_protection_kernel__dignity_reading, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
