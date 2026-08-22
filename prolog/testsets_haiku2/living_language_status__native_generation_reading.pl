% ============================================================================
% CONSTRAINT STORY: living_language_status__native_generation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_living_language_status__native_generation_reading, []).

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
 *   constraint_id: living_language_status__native_generation_reading
 *   human_readable: Living Language Status: Native Generation Reading
 *   domain: sociolinguistic/nationalist/religious
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of a contested kernel: the
 *   definition of 'living language status.' The native-generation reading
 *   asserts that a language is 'living' only if native speakers transmit it
 *   generationally as a mother tongue in daily life — framing liturgical
 *   recitation as 'preservation of a corpse, not vitality.' This reading
 *   legitimizes secular nationalist language policy by providing what appears
 *   to be a scientific, objective metric (native-speaker acquisition rates)
 *   for determining which languages deserve state investment and
 *   institutional support. The reading simultaneously delegitimizes
 *   liturgical-only and literary transmission modes by positioning them as
 *   insufficient or inauthentic. The structure is tangled: genuine
 *   coordination function (measuring language transmission is useful for
 *   policy) coupled with asymmetric extraction (authority and resources flow
 *   to native-generation modes, liturgical modes are framed as failures). The
 *   claim/metric gap is intentional and observable: the constraint is CLAIMED
 *   as tangled_rope (combining coordination and extraction) while suppression
 *   and theater metrics rise substantially over the 1900–2025 interval,
 *   indicating increasing emphasis on enforcement and performative
 *   legitimacy-seeking rather than coordination.
 *
 * KEY AGENTS:
 *   - Secular nationalist movements: organize state policy around native-language transmission; gain authority by claiming alignment with linguistic science
 *   - Liturgical-only language communities: maintain language through prayer and ritual study but lose institutional legitimacy under the native-generation reading
 *   - State education authorities: implement the criterion through curriculum and teacher training; deploy state resources accordingly
 *   - Linguistic science community: operates under a disciplinary consensus that native acquisition is the primary vitality marker; grants authority to nationalist policy
 *   - Daily monolingual speakers: positioned as the model of authentic vitality; receive institutional validation and support
 *   - Literary tradition carriers: excluded from conversation about vitality; their intellectual and creative transmission is dismissed as inauthentic if not anchored in native-speaker child acquisition
 *   - Diaspora and minority populations: trapped in identity-locked transmission contexts; their partial transmission is deemed insufficient despite effort and commitment
 *   - Revitalization linguists and activists: take testimony from multiple stakeholder positions; produce counter-evidence on transmission modes and linguistic outcomes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(living_language_status__native_generation_reading, 0.58).
domain_priors:suppression_score(living_language_status__native_generation_reading, 0.62).
domain_priors:theater_ratio(living_language_status__native_generation_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(living_language_status__native_generation_reading, tangled_rope).
narrative_ontology:human_readable(living_language_status__native_generation_reading, "Living Language Status: Native Generation Reading").
narrative_ontology:topic_domain(living_language_status__native_generation_reading, "sociolinguistic/nationalist/religious").

domain_priors:requires_active_enforcement(living_language_status__native_generation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(living_language_status__native_generation_reading, 'b2717e7b-6ab3-442d-9def-35348274d0a3').
narrative_ontology:cs_kernel_codification('b2717e7b-6ab3-442d-9def-35348274d0a3', distributed).
narrative_ontology:cs_authority_grounding('b2717e7b-6ab3-442d-9def-35348274d0a3', extraction).
narrative_ontology:cs_reading_relation('b2717e7b-6ab3-442d-9def-35348274d0a3', living_language_status__liturgical_preservation_reading, coexists_with).
narrative_ontology:cs_reading_relation('b2717e7b-6ab3-442d-9def-35348274d0a3', living_language_status__literary_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('b2717e7b-6ab3-442d-9def-35348274d0a3', foundational, native_monolingual_childhood_transmission_necessary).
narrative_ontology:cs_axiom_status(native_monolingual_childhood_transmission_necessary, holdable).
narrative_ontology:cs_axiom_grounding('b2717e7b-6ab3-442d-9def-35348274d0a3', native_monolingual_childhood_transmission_necessary, empirically_contingent).
narrative_ontology:cs_axiom('b2717e7b-6ab3-442d-9def-35348274d0a3', secondary, non_native_transmission_modes_insufficient_for_vitality).
narrative_ontology:cs_axiom_status(non_native_transmission_modes_insufficient_for_vitality, holdable).
narrative_ontology:cs_axiom_grounding('b2717e7b-6ab3-442d-9def-35348274d0a3', non_native_transmission_modes_insufficient_for_vitality, empirically_contingent).
narrative_ontology:cs_reference_frame('b2717e7b-6ab3-442d-9def-35348274d0a3', linguistic_vitality_monolingual_native_criterion).
narrative_ontology:cs_drift_state('b2717e7b-6ab3-442d-9def-35348274d0a3', contemporary_2025, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b2717e7b-6ab3-442d-9def-35348274d0a3', '').
narrative_ontology:cs_kernel_id(living_language_status__native_generation_reading, living_language_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(living_language_status__native_generation_reading, secular_nationalist_movements).
narrative_ontology:constraint_victim(living_language_status__native_generation_reading, liturgical_only_language_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(living_language_status__native_generation_reading, state_education_authorities).
narrative_ontology:constraint_beneficiary(living_language_status__native_generation_reading, linguistic_science_community).
narrative_ontology:constraint_beneficiary(living_language_status__native_generation_reading, daily_life_monolingual_speakers).
narrative_ontology:constraint_victim(living_language_status__native_generation_reading, diaspora_minority_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organize state policy and institutional investment around native-language transmission. Frame the native-generation criterion as scientific fact and the natural basis for language policy. Gain authority to determine which languages are 'alive' and deserve support. Set curriculum standards, fund education programs, and deploy media to promote native-language use. Collect prestige and political power by aligning policy with a purportedly objective linguistic metric.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, secular_nationalist_movements, agenda_setter,
    organized, generational, mobile, national).

% Maintain the language through religious prayer, study, and ritual. Use it to transmit sacred texts, interpretive traditions, and community identity across generations — but not as a daily first language for all members. Face institutional stigma: their language is declared 'dead' or 'dying' despite centuries of continuous transmission. Lose access to education funding, media representation, and institutional legitimacy. Feel pressure to abandon their transmission mode in favor of native-only approaches. Cannot exit without dismantling core religious and cultural identity.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, liturgical_only_language_communities, payer,
    moderate, generational, identity_locked, regional).

% Implement the native-generation criterion through school curriculum design, teacher certification, standardized testing of language competency, and allocation of education funding. Gain institutional authority to certify which languages are 'alive' or 'dead' and to direct students toward approved language transmission modes. Benefit from having a clear, quantifiable metric (percentage of children acquiring the language as L1) that justifies policy decisions. Can shift implementation but not easily change the underlying criterion without political resistance.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, state_education_authorities, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(living_language_status__native_generation_reading, state_education_authorities, beneficiary).

% Operates under a disciplinary consensus that native acquisition in early childhood is the primary indicator of language vitality and transmission stability. The native-generation reading aligns with and strengthens this consensus. Gain professional prestige by providing the 'objective' scientific metric that nationalist policy relies on. Are consulted in policy-making, secure funding for research on language acquisition and transmission, and shape education curricula. Can study alternative transmission modes but the mainstream consensus remains fixed on native acquisition.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, linguistic_science_community, beneficiary,
    institutional, biographical, mobile, global).

% Use the language as their primary or sole daily language and transmit it natively to children. Positioned as the model of authentic linguistic vitality by the native-generation criterion. Receive institutional validation through school curriculum, media representation, and cultural prestige. Benefit from state investment in language infrastructure. Face no direct extraction cost, though they may experience pressure to maintain monolingual use even when multilingualism would be functionally advantageous.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, daily_life_monolingual_speakers, beneficiary,
    moderate, biographical, constrained, national).

% Use the language to produce new literature, philosophy, journalism, and intellectual work. Demonstrate linguistic vitality through creative and scholarly productivity. Under the native-generation reading, their contribution is invisible or dismissed as elitist if it is not produced by native daily-use speakers. Are excluded from the conversation about what constitutes language vitality; their transmission mode (through writing, apprenticeship, and intellectual community) is not recognized as legitimate. Would be included if the literary_continuity_reading were adopted instead.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, literary_tradition_carriers, excluded,
    moderate, generational, constrained, global).

% Attempt to transmit a heritage language to children in an environment where the dominant language has institutional weight (schools, markets, government) and the heritage language has little. Most children acquire the dominant language as L1; heritage language transmission often occurs in the home but not in all daily contexts. Under the native-generation criterion, their transmission is deemed insufficient — their language is classified as 'dead' or 'dying' despite multi-generational effort and identity commitment. No viable exit: abandoning transmission means identity loss; continuing transmission is framed as futile. Depend entirely on external institutional support that the criterion systematically withholds.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, diaspora_minority_populations, payer,
    powerless, generational, trapped, global).

% Conduct longitudinal research on language communities using multiple transmission modes (liturgical, multilingual heritage, literary). Document cases where languages maintain complex morphosyntax, cultural knowledge, and communicative function through non-native contexts. Produce counter-evidence to the native-generation criterion. Advocate for multi-modal vitality metrics. Consult with liturgical and diaspora communities. Published findings challenge the nationalist consensus but face resistance from mainstream sociolinguistics and policy-making institutions.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, revitalization_linguists_and_activists, observer,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(living_language_status__native_generation_reading, secular_nationalist_movements).
narrative_ontology:fixing_cost_class(living_language_status__native_generation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Operates a standardized metric for measuring language vitality and directing limited state resources to the strongest transmission modes — without a shared quantifiable criterion, policy-makers cannot prioritize investment and educational effort remains scattered.
% TRANSFER_FUNCTION: Moves institutional authority, education funding, media representation, and social prestige from liturgical-only and literary transmission modes toward native-speaker daily-life transmission; concentrates prestige in monolingual childhood acquisition while delegitimizing other modalities; transfers influence over language policy from communities to nationalist movements and state education systems.
% ABSENT_VOICES: Liturgical-only communities (structurally excluded from recognition as legitimate language users); diaspora and multilingual heritage speakers (framed as failures rather than included in the conversation); literary and intellectual tradition carriers (dismissed as elitist if not anchored in native-speaker daily use); comparative linguists studying non-native transmission stability (their counter-evidence is published but not incorporated into policy); indigenous communities using alternative transmission modes that do not fit the native-monolingual model. These voices would argue that language vitality is multi-modal and cannot be reduced to a single metric.
% DISAPPEARANCE_RATIONALE: If the native-generation criterion disappeared and were replaced by a multi-modal vitality metric, nationalist policy would be forced to recognize alternative transmission modes and redistribute resources accordingly — education funding would diversify, media representation would include literary and multilingual voices, and liturgical and diaspora communities would regain institutional legitimacy. The underlying question (how to measure language vitality for policy) would remain, but the specific mechanism (native-speaker rates as the sole criterion) would be gone. Some claim the world would rearrange (resources would flow to neglected communities); others claim it would remain roughly the same (native transmission is naturally the strongest mode anyway). The disagreement is genuine and reflects the kernel contest.
% FOUNDING_PROBLEM: Early-to-mid 20th century language policy needed a quantifiable, operationalizable metric to distinguish languages worth preserving through institutional investment from languages in terminal decline. Educators and linguists needed a measurable standard for educational planning: which languages should receive school curriculum investment? Native-speaker acquisition rates emerged as an apparently objective marker of transmission viability because early studies showed strong correlation between childhood native acquisition and long-term language stability.
% FOUNDING_PROBLEM_CORROBORATION: Sociolinguists and linguists working on language endangerment (outside the nationalist policy beneficiary set) confirm the founding problem was real: mid-20th century policy-makers did need operational metrics for language investment decisions. However, these same researchers have documented that native-acquisition rate, while empirically predictive in high-pressure contexts, is not the ONLY indicator of linguistic transmission viability — languages maintained through literacy, ritual, and multilingual code-switching also preserve grammatical complexity and cultural meaning. The founding problem (identifying endangered languages) remains live; the proposed solution (native-acquisition as the sole vitality metric) is increasingly challenged as incomplete or culturally biased.
narrative_ontology:disappearance_verdict(living_language_status__native_generation_reading, contested).
narrative_ontology:founding_problem_status(living_language_status__native_generation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(living_language_status__native_generation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(living_language_status__native_generation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(living_language_status__native_generation_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(living_language_status__native_generation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(living_language_status__native_generation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(living_language_status__native_generation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58 at 2025) reflects that the native-generation criterion transfers authority and resources away from liturgical and literary modes without providing those modes with alternative paths to legitimacy. Suppression (0.62) is high because the constraint's persistence depends on actively excluding and delegitimizing alternative transmission modes, not on participant preference for monolingual childhood acquisition. Theater (0.41, rising from 0.08 in 1900) indicates growing performative emphasis: the state increasingly invokes the 'native speaker' metric in policy-making and media, partly to justify extraction, partly because the metric has become institutionally embedded. The measurement series runs on one shared grid (1900, 1945, 1970, 1995, 2015, 2025) so every tracked metric is authored at every examined time point. The rising trajectory from 1900 (low institutional pressure) to 1945 (post-nationalism, linguistic science codifies native-acquisition standard) to 2025 (established policy baseline) shows the constraint accumulating extractive force as nationalism matured and the native-generation criterion became self-evident to policy-makers. Accessibility_collapse (0.48) reflects that liturgical and literary transmission modes persist despite institutional pressure — alternatives are not fully foreclosed, they are strategically delegitimized. Resistance (0.71) is high because liturgical communities, diaspora populations, and counter-evidence from linguistic research actively contest the native-generation criterion.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (liturgical, diaspora) and the agenda-setter seats (nationalist, state, science) compute to different constraint types: from the agenda-setter perspective, the constraint coordinates language measurement and directs policy efficiently (tangled_rope, high coordination benefit). From the payer perspective, the constraint extracts authority and delegitimizes their transmission mode while offering no path to rehabilitation (snare, pure extraction with a coordination cover story). The engine's per-seat classification reflects this: an institutional or nationalist seat computes the type as tangled_rope; a liturgical or diaspora seat computes it as snare. The directionality divergence (d ranges from ~0.15 for nationalist beneficiaries to ~0.88 for liturgical victims) creates this seat-level type gap, which is exactly the measurement the constraint-story corpus exists to capture.
 *
 * DIRECTIONALITY LOGIC:
 *   Secular nationalist movements benefit from the criterion because it legitimizes their policy priorities and grants them authority over language certification — they set the agenda for what counts as 'real' vitality. They are not constrained by external verification; they are 'mobile' in the sense that they can shift policy focus if needed. State education authorities are institutional agenda-setters who implement the criterion and collect institutional power from it. Linguistic science operates as a beneficiary (gains prestige by providing the 'objective' metric) but with secondary agenda-setter power (they certify which languages are alive or dead). Liturgical-only communities are victims because they bear the cost of institutional delegitimization: their children are redirected to state schools teaching native-only curriculum, their language loses funding and media support, and they face social pressure to abandon 'dead language' status. They are identity-locked — abandoning liturgical transmission means abandoning core religious and cultural identity. Diaspora populations are powerless victims, trapped without arbitrage options: they try to transmit heritage language but the institutional standard (native monolingual childhood) is nearly impossible in multilingual contexts, and their effort is declared insufficient. Daily monolingual speakers are beneficiaries but not agenda-setters — they gain validation but do not organize policy. Literary tradition carriers are excluded — they would argue that intellectual and creative transmission sustains vitality, but their contribution is invisible under the native-generation reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (identifying which languages are endangered and deserve preservation support) is contestable but not dead — linguists and policy-makers still use vitality metrics to allocate resources. The founding problem status is correctly authored as 'contested' because the native-generation criterion itself is increasingly challenged (by liturgical communities, diaspora activists, revitalization linguists studying multilingual transmission). The disappearance verdict is 'contested' because the stakes are genuinely disputed: nationalist policy would redistribute resources if the criterion disappeared; liturgical communities argue their language would recover legitimacy; linguists would need alternative metrics. The constraint avoids the piton trap (dead function maintained by theater) because the native-generation criterion still shapes real policy and education investment — it is not purely performative. But the rising theater ratio (0.08 to 0.41) and the multiple omega variables documenting alternative transmission modes suggest the constraint is approaching Goodhart drift: the metric ('native-speaker rates') is becoming decoupled from the underlying goal (preserving languages that carry culture and meaning), and the system increasingly optimizes for the metric while the substantive function (meaningful linguistic transmission) may be served equally well by other modes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is native-speaker generational transmission the correct metric for language vitality, or is it one contested criterion among several defensible modes (liturgical, literary, scholarly)?',
    'Empirical: longitudinal studies of language communities using multiple transmission modes (liturgical, literary, multilingual heritage contexts) to track morphosyntactic stability, transmission of cultural knowledge, and intergenerational literacy. Conceptual: philosophical analysis of what ''living'' means for a language — does it require daily monolingual use, or can it survive through multiple modalities?',
    'If native-generation transmission is the ONLY defensible criterion, the constraint is structurally sound and liturgical-only communities genuinely do preserve ''dead'' languages. If multiple modes can sustain linguistic vitality, the native-generation criterion is a contestable reading that privileges nationalist policy over other transmission traditions, and the constraint reclassifies from tangled_rope (coordinating and extracting) toward snare (purely extractive under a false coordination story).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the native-generation reading is structurally privileged or one contested reading among several.').

omega_variable(
    suppression_mechanism_internalization,
    'Do liturgical-only communities experience suppression primarily through institutional denying of resources (structural), or through internalized shame and identity degradation (''dead language'' framing)?',
    'Ethnographic study of community self-perception: if communities internalize the ''corpse'' narrative and cease transmission efforts voluntarily, suppression is partly internalized. If communities maintain transmission despite institutional penalties (low school funding, social stigma, no media), suppression is primarily external and resisted.',
    'If suppression is primarily internalized, the constraint''s effective extraction is higher than the scalar suppression metric suggests — the target carries the suppression after institutional pressure is removed (post-exit persistence). This supports reclassification toward snare. If suppression is primarily structural, it operates as designed and remains within the tangled_rope frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of liturgical transmission is structural or internalized.').

omega_variable(
    linguistic_science_independence,
    'Is the consensus among academic linguists about native-speaker acquisition as the vitality criterion truly independent of nationalist policy influence, or has nationalist funding and institutional pressure shaped the disciplinary consensus?',
    'Historical analysis of funding flows to linguistics departments and research centers during nationalist language revitalization periods; interviews with linguists from outside the nationalist beneficiary regions; comparison of vitality metrics used in non-nationalist linguistic contexts.',
    'If the linguistic consensus is contaminated by nationalist funding, the beneficiary claims authority over certification of vitality, and the constraint reclassifies toward snare (false coordination: the science is organized to support extractive policy). If the consensus is genuinely independent, the reading gains legitimacy from the scientific seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(linguistic_science_independence, empirical, 'Whether linguistic science regarding vitality metrics is independent of nationalist influence.').

omega_variable(
    liturgical_transmission_morphosyntactic_stability,
    'Do languages maintained purely through liturgical transmission exhibit morphosyntactic degradation, or do they preserve complex grammar through non-native-speaker contexts (scribal tradition, apprenticeship, textual study)?',
    'Comparative morphosyntactic analysis of liturgical-only languages (Biblical Hebrew, Church Latin, Sanskrit in Vedic ritual, Classical Arabic) against historical records and contemporary native-speaker languages; measurement of complexity retention across generations without daily monolingual use.',
    'If liturgical transmission preserves morphosyntactic complexity, it sustains a core linguistic function (grammar and meaning-making) without native-speaker daily use, which undermines the native-generation criterion''s claim to uniqueness. If morphosyntax degrades without daily monolingual transmission, the criterion''s empirical basis is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(liturgical_transmission_morphosyntactic_stability, empirical, 'Whether non-native liturgical transmission preserves grammatical complexity.').

omega_variable(
    nationalist_policy_contingency,
    'Would the native-generation criterion have crystallized as the standard of linguistic vitality if nationalist policy had not created institutional demand for an operationalizable metric of language authenticity?',
    'Intellectual history: trace the emergence of vitality metrics in sociolinguistics alongside nationalist language policies (Haskalah revival movements, post-colonial nation-building, indigenous-language revitalization funding criteria). Identify whether the native-acquisition criterion emerged from disciplinary linguistics independently or in response to policy demand.',
    'If the criterion emerged primarily in response to nationalist policy demand, it is a constructed reading serving nationalist interests, not a neutral scientific finding — the constraint is extractive and the ''science'' is the cover story. If it emerged independently from linguistic research, the constraint has stronger legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nationalist_policy_contingency, conceptual, 'Whether the native-generation criterion is nationalist-policy-driven or independently scientific.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(living_language_status__native_generation_reading, 1900, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(livi_tr_t1900, living_language_status__native_generation_reading, theater_ratio, 1900, 0.08).
narrative_ontology:measurement(livi_tr_t1945, living_language_status__native_generation_reading, theater_ratio, 1945, 0.15).
narrative_ontology:measurement(livi_tr_t1970, living_language_status__native_generation_reading, theater_ratio, 1970, 0.28).
narrative_ontology:measurement(livi_tr_t1995, living_language_status__native_generation_reading, theater_ratio, 1995, 0.38).
narrative_ontology:measurement(livi_tr_t2015, living_language_status__native_generation_reading, theater_ratio, 2015, 0.4).
narrative_ontology:measurement(livi_tr_t2025, living_language_status__native_generation_reading, theater_ratio, 2025, 0.41).

% Extraction over time
narrative_ontology:measurement(livi_be_t1900, living_language_status__native_generation_reading, base_extractiveness, 1900, 0.22).
narrative_ontology:measurement(livi_be_t1945, living_language_status__native_generation_reading, base_extractiveness, 1945, 0.38).
narrative_ontology:measurement(livi_be_t1970, living_language_status__native_generation_reading, base_extractiveness, 1970, 0.52).
narrative_ontology:measurement(livi_be_t1995, living_language_status__native_generation_reading, base_extractiveness, 1995, 0.56).
narrative_ontology:measurement(livi_be_t2015, living_language_status__native_generation_reading, base_extractiveness, 2015, 0.57).
narrative_ontology:measurement(livi_be_t2025, living_language_status__native_generation_reading, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(livi_su_t1900, living_language_status__native_generation_reading, suppression_requirement, 1900, 0.18).
narrative_ontology:measurement(livi_su_t1945, living_language_status__native_generation_reading, suppression_requirement, 1945, 0.35).
narrative_ontology:measurement(livi_su_t1970, living_language_status__native_generation_reading, suppression_requirement, 1970, 0.52).
narrative_ontology:measurement(livi_su_t1995, living_language_status__native_generation_reading, suppression_requirement, 1995, 0.62).
narrative_ontology:measurement(livi_su_t2015, living_language_status__native_generation_reading, suppression_requirement, 2015, 0.62).
narrative_ontology:measurement(livi_su_t2025, living_language_status__native_generation_reading, suppression_requirement, 2025, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(living_language_status__native_generation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(living_language_status__native_generation_reading, 0.12).
narrative_ontology:affects_constraint(living_language_status__native_generation_reading, living_language_status__liturgical_preservation_reading).
narrative_ontology:affects_constraint(living_language_status__native_generation_reading, living_language_status__literary_continuity_reading).

% DUAL FORMULATION NOTE:
% The three constraint stories (native_generation_reading, liturgical_preservation_reading, literary_continuity_reading) form a constraint family instantiating three competing readings of the single kernel 'living_language_status.' Each reading has its own ε (extractiveness), its own beneficiary/victim structure, and its own claimed type. The native-generation reading (this file) claims tangled_rope and ε=0.58; it serves as the agenda-setter reading whose policy influence flows to the other two. The liturgical reading claims rope (genuine coordination without extraction) and lower ε; the literary reading claims rope and lower ε. All three should link via network.affects_constraints to establish the constraint family's internal dependency structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(living_language_status__native_generation_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
