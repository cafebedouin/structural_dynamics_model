% ============================================================================
% CONSTRAINT STORY: anthropological_record__naturalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_anthropological_record__naturalist_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: anthropological_record__naturalist_reading
 *   human_readable: Naturalist Scientific Reading of Anthropological Record
 *   domain: epistemology/philosophy_of_science/anthropology
 *
 * SUMMARY:
 *   The naturalist reading instantiates one interpretation of the
 *   anthropological record: human origins are fundamentally material
 *   (evolution, migration, genetic ancestry), knowable through scientific
 *   method, and excludes supernatural causation and non-materialist
 *   explanatory frameworks from legitimate interpretation. This reading
 *   coordinates a unified research program and has produced extraordinary
 *   empirical success in predicting fossil sequences, genetic relationships,
 *   and migration patterns. Simultaneously, the reading operates as an
 *   enforced gatekeeper: non-credentialed interpreters, indigenous knowledge
 *   traditions, and theological frameworks are systematically suppressed from
 *   publication, funding, and institutional authority—not through explicit
 *   prohibition but through credentialing gatekeeping and methodological
 *   exclusion. The constraint is claimed as tangled_rope (genuine
 *   coordination function + active enforcement of asymmetric extraction from
 *   non-naturalist readers). The authored metrics describe rising suppression
 *   (0.71 at interval end) and rising theater_ratio (0.42), indicating the
 *   constraint's functional core is gradually being overlaid with
 *   performative maintenance—increasingly the suppression exists to defend
 *   naturalist institutional monopoly rather than to maintain the
 *   coordination function itself.
 *
 * KEY AGENTS:
 *   - credentialed_scientific_community: institutional beneficiary, agenda-setter, holds interpretive authority via peer review and credentialing
 *   - institutional_academia: institutional beneficiary, outsources legitimacy judgment to scientific establishment while gatekeeping credentials
 *   - non_credentialed_interpreters: powerless victims, excluded from institutional access unless they adopt naturalist framing
 *   - indigenous_knowledge_holders: organized payers, constrained exit (must translate knowledge into naturalist vocabulary or lose authority over ancestral narratives)
 *   - creationist_scholars: moderate-power payers, identity-locked exit (theological commitment vs. scholarly standing)
 *   - competing_interpretive_traditions: excluded from institutional conversation, trapped by structural gatekeeping rather than explicit prohibition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(anthropological_record__naturalist_reading, 0.68).
domain_priors:suppression_score(anthropological_record__naturalist_reading, 0.71).
domain_priors:theater_ratio(anthropological_record__naturalist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(anthropological_record__naturalist_reading, tangled_rope).
narrative_ontology:human_readable(anthropological_record__naturalist_reading, "Naturalist Scientific Reading of Anthropological Record").
narrative_ontology:topic_domain(anthropological_record__naturalist_reading, "epistemology/philosophy_of_science/anthropology").

domain_priors:requires_active_enforcement(anthropological_record__naturalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(anthropological_record__naturalist_reading, '47e4de3d-af44-416c-921a-520e115339b9').
narrative_ontology:cs_kernel_codification('47e4de3d-af44-416c-921a-520e115339b9', distributed).
narrative_ontology:cs_authority_grounding('47e4de3d-af44-416c-921a-520e115339b9', extraction).
narrative_ontology:cs_interpretation_layer_present('47e4de3d-af44-416c-921a-520e115339b9').
narrative_ontology:cs_reading_relation('47e4de3d-af44-416c-921a-520e115339b9', anthropological_record__creationist_reading, coexists_with).
narrative_ontology:cs_reading_relation('47e4de3d-af44-416c-921a-520e115339b9', anthropological_record__indigenous_epistemology_reading, coexists_with).
narrative_ontology:cs_axiom('47e4de3d-af44-416c-921a-520e115339b9', foundational, methodological_naturalism_sole_epistemic_legitimacy).
narrative_ontology:cs_axiom_status(methodological_naturalism_sole_epistemic_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('47e4de3d-af44-416c-921a-520e115339b9', methodological_naturalism_sole_epistemic_legitimacy, empirically_contingent).
narrative_ontology:cs_axiom('47e4de3d-af44-416c-921a-520e115339b9', foundational, materialist_causation_complete_human_origins).
narrative_ontology:cs_axiom_status(materialist_causation_complete_human_origins, holdable).
narrative_ontology:cs_axiom_grounding('47e4de3d-af44-416c-921a-520e115339b9', materialist_causation_complete_human_origins, empirically_contingent).
narrative_ontology:cs_reference_frame('47e4de3d-af44-416c-921a-520e115339b9', scientific_materialism_framework).
narrative_ontology:cs_drift_state('47e4de3d-af44-416c-921a-520e115339b9', contemporary_institutional_consolidation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('47e4de3d-af44-416c-921a-520e115339b9', '').
narrative_ontology:cs_kernel_id(anthropological_record__naturalist_reading, anthropological_record).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, credentialed_scientific_community).
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, institutional_academia).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, non_credentialed_interpreters).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, indigenous_knowledge_holders).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, creationist_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, public_science_audiences).
narrative_ontology:constraint_vindicates(anthropological_record__naturalist_reading, evolutionary_materialism).
narrative_ontology:constraint_vindicates(anthropological_record__naturalist_reading, methodological_naturalism).
narrative_ontology:constraint_vindicates(anthropological_record__naturalist_reading, empiricist_epistemology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls peer review, journal publication, funding allocation, and credentialing in anthropology, paleoanthropology, and evolutionary biology. Enforces methodological naturalism as the non-negotiable frame for admissible interpretations. Sets the terms for what counts as 'evidence,' what questions are 'scientifically legitimate,' and whose interpretations earn institutional standing. Collects prestige, funding, and professional authority through monopoly on the legitimate reading.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, credentialed_scientific_community, agenda_setter,
    institutional, generational, arbitrage, global).

% Universities, research institutes, and funding bodies benefit from exclusive custody of the 'settled science' narrative. Naturalist framing justifies curriculum authority, degree credentialing, and research funding streams. The constraint's enforcement (excluding alternative readings) protects the institution's monopoly on legitimate knowledge production without requiring continuous justification of every exclusion.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, institutional_academia, beneficiary,
    institutional, generational, mobile, global).

% Amateur scholars, independent researchers, and lay interpreters of the anthropological record are systematically excluded from publication, conference presentation, and public authority unless they adopt naturalist framing and earn credentials through accredited institutions. Their interpretations are labeled 'pseudoscience,' 'unscientific,' or 'fringe' regardless of evidentiary engagement. Exit requires abandoning identity as an interpreter of human origins—renouncing their own reading of the evidence.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, non_credentialed_interpreters, payer,
    powerless, biographical, identity_locked, global).

% Indigenous peoples who maintain origin narratives grounded in oral tradition, relational continuity with place, and kinship with non-human beings are told their knowledge is not 'scientific' and cannot count as legitimate interpretation of the anthropological record. They may participate only by translating their knowledge into naturalist vocabulary (which often inverts its meaning). Their access to ancestral remains, sacred sites, and the authority to interpret their own histories is mediated through institutional gatekeeping that privileges naturalist readings.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, indigenous_knowledge_holders, payer,
    organized, civilizational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(anthropological_record__naturalist_reading, indigenous_knowledge_holders, excluded).

% Scholars who read the anthropological record through theological or design-intentionality frameworks find their interpretations excluded from mainstream publication, funding, and institutional employment. Some maintain parallel publishing and speaking platforms; others experience career penalties for public advocacy of non-naturalist readings. Exit requires abandoning their theological commitment or their scholarly standing—an identity fusion that makes exit prohibitively costly.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, creationist_scholars, payer,
    moderate, biographical, identity_locked, regional).

% General audiences learn the anthropological record exclusively through naturalist framing in textbooks, documentaries, and popular science. They are not exposed to the interpretive contest or the existence of alternative readings (unless they actively seek them outside mainstream channels). They benefit from a coherent, accessible narrative of human origins; they also bear the cost of not knowing what they are not being told.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, public_science_audiences, beneficiary,
    powerless, biographical, constrained, global).

% Government agencies, private foundations, and research councils allocate funds only to naturalist-framed research programs. This gatekeeping is not explicit ('no creationist research') but structural (funding criteria require peer review, publication in naturalist journals, alignment with institutional consensus). Funding bodies benefit from outsourcing legitimacy judgments to the scientific establishment while maintaining plausible deniability about excluding alternative readings.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, funding_bodies, agenda_setter,
    institutional, generational, mobile, global).

% Theological, indigenous, and design-intentionality frameworks are excluded from the conversation about what the anthropological record means, not through explicit prohibition but through systematic denial of institutional access. They cannot publish in major journals, cannot obtain research funding from accredited sources, cannot train students in universities, and cannot participate in public authority over interpretation without first converting to naturalist framing.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, competing_interpretive_traditions, excluded,
    moderate, civilizational, trapped, global).

% Philosophers of science and epistemologists study the naturalist reading's claims about scientific method, evidence, and the scope of legitimate explanation. They ask whether methodological naturalism is an empirical finding or an a priori commitment, whether the anthropological record alone justifies materialist conclusions, and whether institutional credentialing tracks truth or power.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, science_philosophers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(anthropological_record__naturalist_reading, credentialed_scientific_community).
narrative_ontology:fixing_cost_class(anthropological_record__naturalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates interpretation of the anthropological record (fossils, genetic data, archaeological sites, migration patterns) into a unified narrative framework so that different researchers produce compatible claims, predictions, and research programs rather than incommensurable accounts.
% TRANSFER_FUNCTION: Moves interpretive authority from non-credentialed readers (indigenous knowledge holders, theologians, independent researchers) to the credentialed scientific establishment. Also redistributes funding, publication access, and professional standing from non-naturalist readers to naturalist readers. Redistributes public authority to speak about human origins from diverse traditions to the scientific community.
% ABSENT_VOICES: Indigenous knowledge holders are partially present but under-represented in the framing conversation. Theological and design-intentionality scholars are explicitly excluded from peer-review publication and mainstream funding. The public audience is absent from the contest — they receive the naturalist reading as settled fact without exposure to the interpretive alternatives that other seats vigorously defend.
% DISAPPEARANCE_RATIONALE: Naturalist readers claim the world would rearrange: without scientific method as the arbiter of legitimate interpretation, scholarship would fragment into incommensurable traditions with no way to adjudicate disputes or accumulate reliable knowledge about human origins. Creationist and indigenous epistemology readers claim the world would rearrange differently: institutional credentialing would lose its monopoly, diverse interpretations would proliferate without suppression, and indigenous peoples would regain authority over their own origin narratives. The verdict hinges on whether the naturalist reading's coordination function is genuinely necessary (rearrangement would be costly) or whether it serves primarily to concentrate interpretive authority (removal would redistribute power but not fragment knowledge).
% FOUNDING_PROBLEM: Early anthropology in the 18th-19th centuries faced diverse, contradictory accounts of human origins from theology, indigenous tradition, and fragmentary fossil evidence. Naturalist method was developed to produce a unified, testable, progressively-refined account that could integrate new evidence without requiring re-negotiation of fundamental premises each time a fossil was discovered.
% FOUNDING_PROBLEM_CORROBORATION: Naturalist scholars attest the founding problem is still live: without methodological naturalism, new discoveries (DNA sequencing, fossil finds, migration data) would be interpreted within dozens of incommensurable frameworks, preventing cumulative knowledge. Indigenous scholars and theologians attest the founding problem was always mis-stated: the assumption that unified narrative within one tradition is necessary (rather than a choice to privilege one tradition) was built into the problem definition itself. They argue the founding problem was 'How do we exclude non-naturalist readings' rather than 'How do we coordinate knowledge about origins.' Independent philosophers of science (Kuhn, Feyerabend, Latour) support the second reading: the coordination was achieved by institutional gatekeeping that defined non-naturalist work as 'not science,' not by the method itself.
narrative_ontology:disappearance_verdict(anthropological_record__naturalist_reading, contested).
narrative_ontology:founding_problem_status(anthropological_record__naturalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(anthropological_record__naturalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(anthropological_record__naturalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(anthropological_record__naturalist_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(anthropological_record__naturalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(anthropological_record__naturalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(anthropological_record__naturalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.45 to 0.68 over the interval, tracking the institutional consolidation of naturalist authority. At t=0 (early 20th century), alternative readings still competed for credibility in some academic spaces; credentialing gatekeeping existed but was less formalized. By t=50 (contemporary), the institutional barriers to non-naturalist publication, funding, and employment have hardened—extractiveness is high not because the empirical case for naturalism is stronger (it is), but because the suppression machinery has intensified. Suppression rises from 0.38 to 0.71 over the same interval: active enforcement effort (journal rejections, funding denials, career penalties) required to exclude alternative readings has increased dramatically as competing traditions (indigenous epistemology, creationist scholarship) have become more academically sophisticated and thus harder to dismiss on grounds of mere incompetence. Theater_ratio rises from 0.18 to 0.42, indicating the performative maintenance of naturalist monopoly (rhetorical claims about 'scientific method' and 'evidence' that increasingly serve institutional gatekeeping rather than empirical adjudication) has grown substantially relative to the original coordination function. The shared measurement grid ensures every metric is authored at every time point, preventing the OQ-105 misalignment pathology.
 *
 * PERSPECTIVAL GAP:
 *   The naturalist scientific community experiences this constraint as genuine coordination: it believes the empirical case for evolution and materialism is overwhelming, methodological naturalism is the only rigorous epistemology, and suppression of non-naturalist readings is justified by their lack of scientific merit. From this seat, suppression is not extraction but hygiene. Non-credentialed interpreters, indigenous knowledge holders, and creationist scholars experience the same constraint as enforced extraction: they believe the anthropological record is genuinely ambiguous, methodological naturalism is an a priori commitment masquerading as empirical discovery, and institutional gatekeeping suppresses alternative readings not because they are unmeritorious but because they threaten the naturalist monopoly. From these seats, suppression is not hygiene but power. The engine will compute per-seat classifications from the structural data: from the naturalist-community seat, the constraint may compute as rope (genuine coordination with low suppression extraction); from the excluded-interpreter seat, it computes as snare or tangled_rope (high extraction, high suppression). This divergence is the measurement the corpus exists to capture.
 *
 * DIRECTIONALITY LOGIC:
 *   Credentialed scientific community: d ≈ 0.1-0.2 (full beneficiary, arbitrage-level exit, powerful institutional position). Institutional academia: d ≈ 0.15-0.25 (beneficiary, mobile exit). Non-credentialed interpreters: d ≈ 0.8-0.9 (full targets, identity-locked exit, powerless position). Indigenous knowledge holders: d ≈ 0.75-0.85 (victims despite organized power, because constrained exit and civilizational time horizon mean they cannot easily abandon the authority to interpret their own origins). Creationist scholars: d ≈ 0.70-0.80 (victims despite moderate power, because identity-locked exit makes the cost of exit prohibitive even if they have moderate resources). These directionality assignments feed the engine's effective extraction computation: for the targets (non-credentialed, indigenous, creationist), effective extraction is amplified by their trapped/identity-locked exit and their powerlessness or constrained power; for the beneficiaries, effective extraction is damped (inverted into subsidy) because they hold power and arbitrage exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—how to coordinate unified, testable interpretation of the anthropological record across diverse traditions—is not dead, but it has been substantially displaced by a different mandate: how to maintain the naturalist reading's institutional monopoly against increasingly credible alternative readings. The measurement series tracking rising suppression (0.38→0.71) and rising theater_ratio (0.18→0.42) captures this displacement. At t=0, suppression was lower because naturalist interpretations were genuinely more parsimonious and alternative readings were less academically articulate. By t=50, suppression has increased not because the empirical case for naturalism is stronger but because alternative readings have become more sophisticated and thus require more active exclusion. The theater_ratio rising to 0.42 indicates that 42% of the constraint's maintenance activity is now performative (rhetorical claims, credential-circulation, journal editor gatekeeping) rather than evidentiary adjudication. This is the mandatrophy signature: the founding coordination function is being overlaid with institutional rent-seeking. The constraint should be classified tangled_rope because it retains a real coordination function (naturalist method does produce integrated, testable, predictively powerful accounts) AND asymmetric extraction (non-naturalist readers are actively suppressed). A snare would be pure extraction (the coordination function would have atrophied); a rope would be coordination without asymmetric extraction (the suppression would be proportionate to enforcing the method, not protecting the monopoly). The rising suppression and theater_ratio suggest the constraint is drifting toward snare as the founding coordination problem atrophies and the constraint becomes primarily extractive gatekeeping.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    methodological_naturalism_contingency,
    'Is methodological naturalism an empirical discovery (the scientific method works because the universe is fundamentally material) or an a priori commitment (we define ''science'' as the study of material causes and exclude anything else as ''unscientific'' by fiat)?',
    'Philosophical analysis of scientific practice and history: does the explanatory success of naturalist science demonstrate that non-material causation is impossible, or only that material-cause explanations are tractable within naturalist methodology? Do cases of successful scientific prediction require the truth of materialism, or only the usefulness of material-cause models?',
    'If methodological naturalism is empirical, the naturalist reading''s exclusion of non-materialist interpretations is justified by evidence. If it is a priori, the exclusion is definitional gatekeeping, and the constraint operates as suppression of alternative epistemologies rather than rejection of false claims. This reframes the entire classification: a constraint that suppresses false-but-academically-articulate readings is different from one that suppresses alternative-but-equally-epistemically-defensible readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(methodological_naturalism_contingency, conceptual, 'Whether naturalism is empirically grounded or a priori commitment.').

omega_variable(
    indigenous_knowledge_suppression_mechanism,
    'Is indigenous knowledge suppressed because it is epistemically inferior (cannot produce reliable predictions about human origins) or because it operates via a different epistemic framework (oral tradition, relational knowing, non-materialist causation) that institutional science defines as ''not science'' regardless of its reliability within its own frame?',
    'Comparative analysis of predictive accuracy, internal coherence, and adaptive utility within indigenous frameworks. Historical-institutional analysis of how indigenous knowledge was deliberately excluded from credentialing and funding systems (not merely outcompeted on evidence). Post-exclusion analysis: if indigenous frameworks were suddenly given equal institutional access, would they produce reliable knowledge or merely incommensurable claims?',
    'If indigenous knowledge is epistemically inferior, suppression is justified by merit. If it is suppressed because it doesn''t fit the naturalist definitional frame, the constraint operates as institutional gatekeeping of epistemology rather than evidence-based adjudication. This would shift classification from rope/tangled-rope (justified coordination) to snare (pure extraction via credentialing monopoly).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_knowledge_suppression_mechanism, empirical, 'Whether indigenous knowledge is suppressed due to epistemic merit or institutional gatekeeping.').

omega_variable(
    credentialing_gatekeeping_necessity,
    'Is institutional credentialing gatekeeping necessary to maintain coherent, rigorous interpretation of the anthropological record, or does it primarily serve to concentrate authority and exclude alternative readings?',
    'Counterfactual analysis: what would happen to the quality and coherence of anthropological knowledge if credentialing gatekeeping were removed and all interpretations (naturalist, creationist, indigenous) competed for funding and publication on equal terms? Would knowledge accumulate or fragment? Would integration of evidence improve or degrade?',
    'If gatekeeping is necessary for coherence, the suppression is justified by epistemic function. If gatekeeping primarily concentrates authority, the constraint is pure extraction (snare) rather than coordination with extraction (tangled_rope). This reframes the entire classification and the justifiability of suppressing non-naturalist readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(credentialing_gatekeeping_necessity, conceptual, 'Whether credentialing gatekeeping is epistemically necessary or primarily extractive.').

omega_variable(
    identity_lock_mechanism,
    'For non-credentialed interpreters, indigenous knowledge holders, and creationist scholars, what specifically makes exit prohibitively costly? Is it structural (economic dependency on credentialed employment) or internalized (fusion of identity with interpretive framework)?',
    'Post-exit trajectory analysis: if constraints removed (credentialing fully devalued, indigenous knowledge given equal institutional standing, creationist scholarship fully legitimized), would trapped/identity-locked agents exit their subordinated positions? Would the suppression metrics drop? Would alternative readings proliferate?',
    'If exit barriers are internalized (identity fusion with interpretive framework), the suppression is sustained by cognition patterns that persist even after structural barriers are removed—this amplifies the effective extraction beyond what structural measures alone would predict. If exit barriers are structural (institutional gatekeeping), removing the gatekeeping would reduce suppression directly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether identity-lock is structural or internalized for non-credentialed interpreters.').

omega_variable(
    kernel_reading_foreclosure,
    'Does the naturalist reading logically foreclose the creationist reading (they cannot coexist in any single framework), or do they coexist as incommensurable but live positions held by different institutional communities?',
    'Logical analysis: does accepting methodological naturalism require denying theological causation in principle, or only declining to use theological explanations within scientific discourse? Can a person hold both that God created humans (theological claim) and that humans share evolutionary ancestry with other primates (scientific claim)?',
    'If foreclosure is logical, the naturalist reading''s exclusion of creationist readings is structural to the framework, not gatekeeping. If they coexist as incommensurable positions, the exclusion is institutional gatekeeping (credentialing monopoly) rather than principled incompatibility.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether naturalist and creationist readings logically foreclose each other or coexist as incommensurable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(anthropological_record__naturalist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anth_tr_t0, anthropological_record__naturalist_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(anth_tr_t0, observed).
narrative_ontology:measurement(anth_tr_t8, anthropological_record__naturalist_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement_basis(anth_tr_t8, observed).
narrative_ontology:measurement(anth_tr_t16, anthropological_record__naturalist_reading, theater_ratio, 16, 0.31).
narrative_ontology:measurement_basis(anth_tr_t16, observed).
narrative_ontology:measurement(anth_tr_t25, anthropological_record__naturalist_reading, theater_ratio, 25, 0.37).
narrative_ontology:measurement_basis(anth_tr_t25, observed).
narrative_ontology:measurement(anth_tr_t35, anthropological_record__naturalist_reading, theater_ratio, 35, 0.4).
narrative_ontology:measurement_basis(anth_tr_t35, observed).
narrative_ontology:measurement(anth_tr_t50, anthropological_record__naturalist_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement_basis(anth_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(anth_be_t0, anthropological_record__naturalist_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(anth_be_t0, observed).
narrative_ontology:measurement(anth_be_t8, anthropological_record__naturalist_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement_basis(anth_be_t8, observed).
narrative_ontology:measurement(anth_be_t16, anthropological_record__naturalist_reading, base_extractiveness, 16, 0.59).
narrative_ontology:measurement_basis(anth_be_t16, observed).
narrative_ontology:measurement(anth_be_t25, anthropological_record__naturalist_reading, base_extractiveness, 25, 0.65).
narrative_ontology:measurement_basis(anth_be_t25, observed).
narrative_ontology:measurement(anth_be_t35, anthropological_record__naturalist_reading, base_extractiveness, 35, 0.67).
narrative_ontology:measurement_basis(anth_be_t35, observed).
narrative_ontology:measurement(anth_be_t50, anthropological_record__naturalist_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement_basis(anth_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(anth_su_t0, anthropological_record__naturalist_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(anth_su_t0, observed).
narrative_ontology:measurement(anth_su_t8, anthropological_record__naturalist_reading, suppression_requirement, 8, 0.48).
narrative_ontology:measurement_basis(anth_su_t8, observed).
narrative_ontology:measurement(anth_su_t16, anthropological_record__naturalist_reading, suppression_requirement, 16, 0.56).
narrative_ontology:measurement_basis(anth_su_t16, observed).
narrative_ontology:measurement(anth_su_t25, anthropological_record__naturalist_reading, suppression_requirement, 25, 0.63).
narrative_ontology:measurement_basis(anth_su_t25, observed).
narrative_ontology:measurement(anth_su_t35, anthropological_record__naturalist_reading, suppression_requirement, 35, 0.68).
narrative_ontology:measurement_basis(anth_su_t35, observed).
narrative_ontology:measurement(anth_su_t50, anthropological_record__naturalist_reading, suppression_requirement, 50, 0.71).
narrative_ontology:measurement_basis(anth_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(anthropological_record__naturalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(anthropological_record__naturalist_reading, 0.12).
narrative_ontology:affects_constraint(anthropological_record__naturalist_reading, anthropological_record__creationist_reading).
narrative_ontology:affects_constraint(anthropological_record__naturalist_reading, anthropological_record__indigenous_epistemology_reading).
narrative_ontology:affects_constraint(anthropological_record__naturalist_reading, credentialing_gatekeeping__scientific_authority).
narrative_ontology:affects_constraint(anthropological_record__naturalist_reading, methodological_naturalism__a_priori_commitment).

% DUAL FORMULATION NOTE:
% The anthropological_record kernel decomposes into three constraint stories, one per reading. The naturalist reading (this story) claims high extractiveness via credentialing gatekeeping and methodological exclusion; the creationist reading authors the constraint from the creationist seat where the naturalist reading is the suppressor; the indigenous reading authors from the indigenous-knowledge seat where both naturalist and creationist readings are colonial impositions. Each story has its own ε (high for naturalist due to institutional gatekeeping, high for creationist due to career penalties, high for indigenous due to land/knowledge dispossession), its own beneficiary/victim structure, and its own type. The three stories are linked by affects_constraints because readings of the same kernel structurally compete: if the creationist reading gained institutional standing, the naturalist reading's exclusivity would drop; if indigenous readings were fully legitimized, the naturalist reading's monopoly would dissolve. All three readings are live positions held by different institutional communities; none logically forecloses the others within a single framework (omega variable documents this). The three stories are siblings in a constraint family, not a single multivalent constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(anthropological_record__naturalist_reading, organized, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
