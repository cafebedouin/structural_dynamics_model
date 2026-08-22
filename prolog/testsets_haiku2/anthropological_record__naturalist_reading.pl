% ============================================================================
% CONSTRAINT STORY: anthropological_record__naturalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Anthropological Record (Naturalist Reading): Materialist Origins via Scientific Method
 *   domain: epistemology/philosophy_of_science/anthropology
 *
 * SUMMARY:
 *   The anthropological record — fossils, genetic markers, archaeological
 *   artifacts, biogeographic patterns — is a contested kernel. The naturalist
 *   reading interprets this record as evidence of materialist human origins
 *   (evolution, migration) knowable via scientific method: shared genetic
 *   ancestry with other primates, progressive fossil sequences showing
 *   anatomical transitions, settlement patterns reconstructible from
 *   artifacts and DNA. This reading has become institutionally dominant via
 *   credentialing gatekeeping (peer review, funding allocation, museum
 *   curation, educational curricula). Alternative readings — creationist
 *   (divine design or creation events), indigenous relational (ancestral
 *   continuity with place via oral transmission) — are simultaneously
 *   suppressed through institutional exclusion and structurally reinterpreted
 *   as non-scientific or mythological. The naturalist reading benefits
 *   credentialed researchers and academic institutions through prestige,
 *   funding, and epistemic authority. It extracts from non-credentialed
 *   interpreters, indigenous knowledge keepers, and religious scholars
 *   through systematic delegitimation. The constraint is CLAIMED as tangled
 *   rope (genuine coordination via shared evidentiary standards PLUS
 *   asymmetric extraction via credentialing gatekeeping). The authored
 *   metrics describe substantial extraction (0.68) and active suppression
 *   (0.72), with moderate theater (0.41): the coordination function is real
 *   but increasingly intertwined with defensive enforcement against
 *   alternatives.
 *
 * KEY AGENTS:
 *   - Evolutionary biologists (institutional beneficiary + agenda setter): control evidentiary standards, peer review gates, funding allocation
 *   - Academic anthropologists (institutional beneficiary): career advancement requires acceptance of naturalist framework
 *   - Non-credentialed interpreters (powerless victims): prevented from legitimate knowledge adjudication about their own frameworks
 *   - Indigenous knowledge keepers (moderate-power victims + excluded): relational knowledge absorbed or suppressed; identity-locked to local contexts
 *   - Religious scholars (moderate-power victims): theological readings excluded from credentialed discourse
 *   - Peer review apparatus (institutional agenda setter): enforces naturalist evidentiary standards through gatekeeping
 *   - Museum/archive curators (institutional agenda setter): control material access and interpretive framing of the record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(anthropological_record__naturalist_reading, 0.68).
domain_priors:suppression_score(anthropological_record__naturalist_reading, 0.72).
domain_priors:theater_ratio(anthropological_record__naturalist_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(anthropological_record__naturalist_reading, tangled_rope).
narrative_ontology:human_readable(anthropological_record__naturalist_reading, "Anthropological Record (Naturalist Reading): Materialist Origins via Scientific Method").
narrative_ontology:topic_domain(anthropological_record__naturalist_reading, "epistemology/philosophy_of_science/anthropology").

domain_priors:requires_active_enforcement(anthropological_record__naturalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(anthropological_record__naturalist_reading, 'ddf9e9cf-1a86-4f99-b416-dc74bf901f02').
narrative_ontology:cs_kernel_codification('ddf9e9cf-1a86-4f99-b416-dc74bf901f02', fixed_text).
narrative_ontology:cs_authority_grounding('ddf9e9cf-1a86-4f99-b416-dc74bf901f02', extraction).
narrative_ontology:cs_interpretation_layer_present('ddf9e9cf-1a86-4f99-b416-dc74bf901f02').
narrative_ontology:cs_reading_relation('ddf9e9cf-1a86-4f99-b416-dc74bf901f02', anthropological_record__creationist_reading, forecloses).
narrative_ontology:cs_reading_relation('ddf9e9cf-1a86-4f99-b416-dc74bf901f02', anthropological_record__indigenous_epistemology_reading, influences).
narrative_ontology:cs_axiom('ddf9e9cf-1a86-4f99-b416-dc74bf901f02', foundational, materialist_causation_exclusivity).
narrative_ontology:cs_axiom_status(materialist_causation_exclusivity, holdable).
narrative_ontology:cs_axiom_grounding('ddf9e9cf-1a86-4f99-b416-dc74bf901f02', materialist_causation_exclusivity, empirically_contingent).
narrative_ontology:cs_axiom('ddf9e9cf-1a86-4f99-b416-dc74bf901f02', foundational, scientific_method_epistemological_monopoly).
narrative_ontology:cs_axiom_status(scientific_method_epistemological_monopoly, holdable).
narrative_ontology:cs_axiom_grounding('ddf9e9cf-1a86-4f99-b416-dc74bf901f02', scientific_method_epistemological_monopoly, deontological).
narrative_ontology:cs_axiom('ddf9e9cf-1a86-4f99-b416-dc74bf901f02', secondary, credentialing_gatekeeping_legitimacy).
narrative_ontology:cs_axiom_status(credentialing_gatekeeping_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('ddf9e9cf-1a86-4f99-b416-dc74bf901f02', credentialing_gatekeeping_legitimacy, instrumental).
narrative_ontology:cs_reference_frame('ddf9e9cf-1a86-4f99-b416-dc74bf901f02', materialist_empirical_sufficiency).
narrative_ontology:cs_drift_state('ddf9e9cf-1a86-4f99-b416-dc74bf901f02', contemporary_epistemological_pluralism_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ddf9e9cf-1a86-4f99-b416-dc74bf901f02', '').
narrative_ontology:cs_kernel_id(anthropological_record__naturalist_reading, anthropological_record).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, evolutionary_biologists).
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, academic_anthropologists).
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, credentialed_researchers).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, non_credentialed_interpreters).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, indigenous_knowledge_keepers).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, religious_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, public_users_of_anthropological_knowledge).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and defend the evidentiary standards for interpreting the anthropological record. Control peer-review processes for journals publishing origins research. Allocate prestige and institutional position through citation and conference inclusion. Directly benefit from the naturalist reading's dominance through funding, tenure, and professional authority. Actively suppress alternative readings through editorial decisions and grant-funding priorities.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, evolutionary_biologists, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(anthropological_record__naturalist_reading, evolutionary_biologists, beneficiary).

% Benefit from institutional frameworks that embed the naturalist reading. Tenure and promotion depend on publication in journals enforcing naturalist standards. Funding institutions prioritize naturalist research agendas. Career advancement requires demonstrating competence within the reading's epistemological framework. Cannot easily exit without losing institutional standing.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, academic_anthropologists, beneficiary,
    institutional, generational, constrained, global).

% Hold PhDs and institutional positions within the credentialing apparatus. Benefit from the gatekeeping that restricts knowledge adjudication to credentialed actors. Can publish, receive funding, and participate in prestigious knowledge-producing institutions. Have exit options but reputational cost is high for those who openly contest the naturalist reading's dominance.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, credentialed_researchers, beneficiary,
    powerful, generational, mobile, global).

% Produce interpretations of the anthropological record through philosophy, theology, or alternative epistemologies. Systematically excluded from institutional knowledge adjudication: journals reject submissions, funding is unavailable, conference participation is denied. Bear the cost of delegitimation and institutional exclusion. Identity-locked: stepping into academic contexts requires accepting naturalist authority or facing dismissal.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, non_credentialed_interpreters, payer,
    powerless, biographical, identity_locked, local).

% Hold knowledge of human continuity with place and ancestry transmitted through oral tradition across generations. This knowledge is either absorbed into naturalist frames (reinterpreted as 'migration narratives' confirming evolutionary timelines) or excluded as 'mythology.' Cannot adjudicate knowledge claims about origins within institutional contexts without translating into naturalist categories. Identity-locked through cultural and relational bonds; exit would abandon ancestral knowledge frameworks.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, indigenous_knowledge_keepers, payer,
    moderate, civilizational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(anthropological_record__naturalist_reading, indigenous_knowledge_keepers, excluded).

% Interpret human origins through theological and scriptural frameworks. Their readings of the anthropological record are systematically suppressed in academic discourse. Can produce scholarship but it is excluded from credentialed knowledge production. Constrained exit: wishing to participate in official knowledge adjudication requires ceding authority to naturalism or accepting institutional exclusion.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, religious_scholars, payer,
    moderate, generational, constrained, regional).

% A non-agent institutional mechanism that enforces the naturalist reading's evidentiary standards through journal gatekeeping. Functions as though naturalist standards are the only legitimate scientific standards, not one reading among contested alternatives. Operationally implements the suppression of competing frameworks.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, peer_review_apparatus, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(anthropological_record__naturalist_reading, peer_review_apparatus).

% Control material access to and interpretive presentation of the anthropological record (fossils, artifacts, remains). Present naturalist interpretations as authoritative. Treat alternative readings as non-factual or subordinate. Their curation choices materially enforce the naturalist reading's dominance by controlling what the public encounters as 'the record.'
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, museum_and_archive_curators, agenda_setter,
    institutional, generational, analytical, global).

% A non-agent institutional structure that embeds the naturalist reading into educational curricula worldwide. Present it as settled fact rather than contested reading. Remove opportunities for students to encounter alternative frameworks. Function as enforcement mechanism for naturalizing the reading across generations.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, public_educational_systems, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(anthropological_record__naturalist_reading, public_educational_systems).

% Allocate research funding overwhelmingly to projects framed within the naturalist reading: evolutionary biology, paleontology, genetics. Deny funding to alternative research agendas. Function as enforcement mechanism controlling resource access and directing scientific labor toward naturalist frameworks.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, funding_bodies, agenda_setter,
    institutional, generational, analytical, global).

% Receive coherent, well-resourced public narratives about human origins from the naturalist reading. Educational systems, museums, and science communication consistently present the naturalist reading as authoritative fact. Benefit from epistemic clarity within the framework. Constrained exit: institutional contexts do not provide alternatives; encountering the reading as the only legitimate account is enforced by resource allocation.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, public_users_of_anthropological_knowledge, beneficiary,
    powerless, biographical, constrained, global).

% Theological, indigenous relational, and philosophical frameworks offering alternative readings are structurally excluded from credentialed knowledge adjudication. Would be able to advance competing claims if institutional gatekeeping opened. Their exclusion is the enforcement mechanism's primary object.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, competing_epistemological_frameworks, excluded,
    moderate, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(anthropological_record__naturalist_reading, competing_epistemological_frameworks).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(anthropological_record__naturalist_reading, evolutionary_biologists).
narrative_ontology:fixing_cost_class(anthropological_record__naturalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, testable, falsifiable framework for interpreting the material record of human origins: genetic evidence, fossil stratigraphy, archaeological dating, and biogeographic patterns. Allows scientists to coordinate on shared evidentiary standards so that empirical claims about human origins can be evaluated against common reference points rather than through incommensurable epistemological frameworks.
% TRANSFER_FUNCTION: Transfers interpretive authority about human origins from multiple frameworks (religious, indigenous relational, philosophical) to a credentialed scientific establishment. Moves resources (research funding, publishing outlets, institutional positions, educational platforms, museum curation authority) to those who accept the naturalist reading's evidentiary standards and methodological presuppositions. Extracts legitimacy and institutional access from non-credentialed interpreters.
% ABSENT_VOICES: Indigenous knowledge keepers would object that the naturalist reading reinterprets or erases their relational knowledge about continuity with ancestors and place. Religious scholars would object that supernatural causation is excluded by definitional fiat, not by evidence. Philosophers of science outside naturalist camps would object that materialism is metaphysical presupposition smuggled in under scientific procedure, not empirically required. These parties are structurally excluded from institutional knowledge adjudication about the very question their frameworks address.
% DISAPPEARANCE_RATIONALE: If the naturalist reading's institutional enforcement disappeared, multiple competing readings would immediately re-enter public discourse. Funding would reallocate to alternative research agendas (theology, indigenous scholarship, phenomenological anthropology). Educational curricula would pluralize. Museums would present multiple readings rather than treating naturalism as authoritative. The monopoly on credentialed knowledge production would dissolve, and the world would rearrange into explicitly contested interpretive frameworks rather than enforced coherence.
% FOUNDING_PROBLEM: In the 19th-20th centuries, naturalist approaches to the anthropological record (fossil anatomy, stratigraphy, evolutionary genetics) resolved profound ignorance about human deep history that theological and philosophical frameworks could not empirically constrain. The reading was built to answer: 'How can we produce testable, falsifiable, empirically updatable claims about human origins rather than remaining in interpretive stalemate between incommensurable worldviews?'
% FOUNDING_PROBLEM_CORROBORATION: Evolutionary biologists and paleontologists attest the founding problem remains live: naturalist methods continue to produce falsifiable, empirically constrained claims that advance human-origins knowledge. Indigenous scholars and theologians attest the founding problem is ill-posed: it presupposes only naturalist methods produce knowledge, thereby constructing the problem it claims to solve. Philosophers of science outside naturalism attest that naturalism's methodological power (empirically fruitful) is conflated with metaphysical necessity (nothing non-material caused human origins). UNESCO deliberations, legislative testimony, and decolonial scholarship document the contested status and the cost of naturalist monopoly on credentialed discourse.
narrative_ontology:disappearance_verdict(anthropological_record__naturalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(anthropological_record__naturalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(anthropological_record__naturalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
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
 *   Extractiveness rises from 0.42 to 0.68 across the interval, documenting a shift from genuine coordination (early naturalist methods solved real ignorance) toward increasingly defensive extraction (modern enforcement against alternatives). Theater ratio stays moderate (0.28→0.41), indicating the coordination function remains real but a growing share of enforcement activity is purely defensive. Suppression accelerates (0.48→0.72), matching the timeline of institutional entrenchment and credentialing hardening. The reading is tangled rope, not pure snare: naturalist standards genuinely coordinate empirical investigation and produce falsifiable claims. But the constraint's persistence now depends increasingly on actively suppressing alternatives through credentialing gatekeeping, not on participant preference. Accessibility collapse is high (0.78): once the naturalist reading is accepted as authoritative, the material record appears to support it exclusively, making alternative readings invisible. Resistance is substantial (0.64): indigenous scholars, theologians, and non-credentialed interpreters actively contest the reading's dominance, but their resistance is systematized into 'non-science' and excluded from institutional adjudication.
 *
 * PERSPECTIVAL GAP:
 *   From the credentialed researcher's seat, the constraint is genuine coordination: shared standards allow scientific progress on human origins. From the non-credentialed interpreter's seat, the same structure is enforced extraction: your reading is delegitimated by fiat, and institutional resources are denied to you regardless of your evidence. The indigenous knowledge keeper's seat is distinct: your knowledge is not 'wrong' by naturalist standards; it is incommensurable, and the constraint functions by refusing to recognize incommensurability as legitimate — your knowledge is absorbed as 'migration stories' (naturalist interpretation) or excluded as 'mythology' (delegitimation). The suppression is not transparent to the beneficiary seats because naturalist standards appear as 'the scientific method' (neutral procedure) rather than as one reading among contested frameworks.
 *
 * DIRECTIONALITY LOGIC:
 *   Evolutionary biologists and academic anthropologists are beneficiaries: the constraint concentrates credentialed authority, funding, and prestige in the naturalist camp. Directionality for institutional seats is toward the beneficiary end (~0.1-0.2). Non-credentialed interpreters and indigenous knowledge keepers are victims: the constraint denies them resources, legitimacy, and participation in knowledge adjudication. Identity-locked exit (cannot abandon relational or theological knowledge without losing identity) places them near the target end (~0.8-0.9). Religious scholars are mid-position (~0.6): they have institutional options (philosophy, theology departments) but cannot participate in anthropological knowledge adjudication without ceding authority to naturalism. Public users sit near symmetric (~0.5): they benefit from coherent educational narratives but carry the cost of epistemic monopoly (no access to alternative readings).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (resolving ignorance about human origins via testable methods) was live when naturalism was one reading among competing frameworks. The problem is now contested: naturalists attest it remains live (methods continue to improve), while non-naturalists attest it is misdefined (it presupposes only naturalism is scientific). Mandatrophy is incipient but not resolved: the constraint persists as enforcement machinery even though its coordination function could in principle be decoupled from suppression (credentialing gates could remain without delegitimating alternatives). The classification as tangled rope rather than snare hinges on whether the coordination function is genuine (testable, falsifiable frameworks ARE valuable) or merely instrumental (serves the institution rather than the inquiry). The measured theater ratio (0.41) suggests the coordination is still real but defense is rising. A mandatrophy resolution would require either (a) loosening credentialing gatekeeping to allow alternative frameworks or (b) abandoning the naturalist reading's aspiration to monopoly authority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_foreclosure,
    'Does the naturalist reading''s core premise (materialist causation is the only legitimate source of knowledge about human origins) logically foreclose the creationist reading (divine causation produced human origins), or do they coexist as incommensurable frameworks held by different parties?',
    'Logical analysis of whether the two premises can be simultaneously held in one framework without contradiction. If a coherent framework exists that includes both, they coexist; if no such framework exists, foreclosure holds.',
    'If foreclosure: the naturalist reading rules out creationism by definition of what counts as knowledge. If coexistence: both readings remain live, and suppression of creationism is an enforcement choice, not a logical necessity. This determines whether the constraint is enforced suppression or genuine incompatibility.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether naturalism logically forecloses or merely excludes theistic origins readings.').

omega_variable(
    credentialing_gatekeeping_necessity,
    'Is the credentialing gatekeeping (Ph.D. requirement, peer review, institutional affiliation) structurally necessary to enforce naturalist standards, or could the reading persist through softer epistemic authority without institutional suppression of alternatives?',
    'Historical counterfactual: in contexts where naturalist frameworks were dominant but credentialing was looser (certain periods, certain regions), did alternative readings proliferate? Empirical: in current open-access and non-academic-gatekept spaces, do the naturalist reading''s evidentiary standards persist or relax?',
    'If gatekeeping is necessary: the constraint''s suppression is intrinsic to the reading''s maintenance. If softer authority would suffice: the measured suppression reflects choice to exclude rather than necessity to maintain the framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credentialing_gatekeeping_necessity, empirical, 'Whether the institutional suppression mechanism is necessary or contingent to the naturalist reading''s persistence.').

omega_variable(
    indigenous_knowledge_integration_path,
    'Can indigenous relational knowledge about human continuity with place be coherently integrated into the naturalist reading (as ''early migration history preserved in oral tradition''), or does integration require fundamentally reframing the knowledge in ways that erase its relational and temporal specificity?',
    'Documented cases where indigenous knowledge was absorbed into naturalist frameworks, paired with indigenous scholarly analysis of whether that absorption preserved or destroyed the knowledge''s meaning and structure.',
    'If integration preserves meaning: apparent suppression may be translation with loss. If integration erases meaning: the constraint''s suppression is the structural cost of maintaining naturalist monopoly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_knowledge_integration_path, conceptual, 'Whether indigenous knowledge can be integrated without losing its distinctive epistemic structure.').

omega_variable(
    materialism_completeness_contestation,
    'Is materialism (the claim that only material causation produces human origins) a vindicated proposition of the naturalist reading, or a presupposition smuggled in under the guise of empirical constraint?',
    'Logical analysis: can a naturalist reading accept material evidence of human origins without accepting the metaphysical claim that nothing non-material caused them? If yes, materialism is extra-empirical.',
    'If materialism is presupposition: the reading vindicates it by enforcement, not by evidence — a false natural law. If it is empirically constrained: the evidence genuinely supports it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(materialism_completeness_contestation, conceptual, 'Whether materialism is empirically required or metaphysically presupposed in the naturalist reading.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the suppression of non-credentialed interpreters maintained by external gatekeeping (institutional exclusion, funding denial, journal rejection), or by internalized epistemic hierarchy (interpreters accept that their frameworks are epistemically inferior)?',
    'Post-exit suppression trajectory: if non-credentialed interpreters are removed from institutional suppression (e.g., indigenous scholars granted funding to work within their own frameworks), do they voluntarily adopt naturalist standards, or do they sustain alternative readings?',
    'If internalized: the constraint''s effective suppression persists even after institutional barriers fall — targets carry the hierarchy with them. If structural: removing gatekeeping would dissolve suppression and reopen the contest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether measured suppression is structural gatekeeping or internalized epistemic hierarchy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(anthropological_record__naturalist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anth_tr_t0, anthropological_record__naturalist_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(anth_tr_t5, anthropological_record__naturalist_reading, theater_ratio, 5, 0.31).
narrative_ontology:measurement(anth_tr_t10, anthropological_record__naturalist_reading, theater_ratio, 10, 0.34).
narrative_ontology:measurement(anth_tr_t15, anthropological_record__naturalist_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement(anth_tr_t20, anthropological_record__naturalist_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(anth_tr_t25, anthropological_record__naturalist_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(anth_tr_t30, anthropological_record__naturalist_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement(anth_tr_t40, anthropological_record__naturalist_reading, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(anth_be_t0, anthropological_record__naturalist_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(anth_be_t5, anthropological_record__naturalist_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(anth_be_t10, anthropological_record__naturalist_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(anth_be_t15, anthropological_record__naturalist_reading, base_extractiveness, 15, 0.6).
narrative_ontology:measurement(anth_be_t20, anthropological_record__naturalist_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement(anth_be_t25, anthropological_record__naturalist_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(anth_be_t30, anthropological_record__naturalist_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(anth_be_t40, anthropological_record__naturalist_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(anth_su_t0, anthropological_record__naturalist_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(anth_su_t5, anthropological_record__naturalist_reading, suppression_requirement, 5, 0.54).
narrative_ontology:measurement(anth_su_t10, anthropological_record__naturalist_reading, suppression_requirement, 10, 0.59).
narrative_ontology:measurement(anth_su_t15, anthropological_record__naturalist_reading, suppression_requirement, 15, 0.64).
narrative_ontology:measurement(anth_su_t20, anthropological_record__naturalist_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(anth_su_t25, anthropological_record__naturalist_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(anth_su_t30, anthropological_record__naturalist_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(anth_su_t40, anthropological_record__naturalist_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(anthropological_record__naturalist_reading, information_standard).
narrative_ontology:boltzmann_floor_override(anthropological_record__naturalist_reading, 0.12).
narrative_ontology:affects_constraint(anthropological_record__naturalist_reading, anthropological_record__creationist_reading).
narrative_ontology:affects_constraint(anthropological_record__naturalist_reading, anthropological_record__indigenous_epistemology_reading).

% DUAL FORMULATION NOTE:
% The anthropological record is a contested kernel with three structurally distinct readings. Each instantiates a different constraint with different ε, beneficiary/victim structures, and types. naturalist_reading (this story) interprets the record via materialist causation and scientific method; creationist_reading interprets it via divine design or creation events; indigenous_epistemology_reading interprets it via relational continuity and oral tradition. These are not different observations of one constraint — they are different constraints instantiated by the same kernel under different readings. The readings coexist and compete in public discourse; none logically forecloses the others, but the naturalist reading structurally influences both by controlling institutional resources and credentialed access. All three stories carry distinct ε values reflecting their different extraction profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
