% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__hanbali_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jurisprudential_method_kernel__hanbali_reading, []).

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
 *   constraint_id: jurisprudential_method_kernel__hanbali_reading
 *   human_readable: Hanbali Jurisprudential Method: Literal Text Primacy with Innovation Exclusion
 *   domain: legal_philosophy/religious_authority/institutional_governance
 *
 * SUMMARY:
 *   The Hanbali jurisprudential reading claims that Islamic law derives from
 *   the literal text of Qur'an and Hadith, interpreted through the opinions
 *   of the Prophet's Companions, with only unanimous consensus (ijma')
 *   recognized as a valid extension. All other reasoning methods —
 *   particularly analogical reasoning (qiyas) and juristic preference
 *   (istihsan) — are characterized as bid'ah (forbidden innovation) that
 *   corrupts the divine kernel. This reading instantiates a specific
 *   constraint on legitimate jurisprudential method. The claim/metric gap is
 *   deliberate and structurally significant: the reading frames itself as
 *   ROPE (genuine coordination preventing jurisprudential chaos through
 *   shared textual method), while the authored metrics describe substantial
 *   extraction (marginalization of rationalist jurists and customary
 *   communities) and significant suppression (institutional exclusion of
 *   alternative reasoning methods). The engine measures this divergence; do
 *   not reconcile the claim to the metrics.
 *
 * KEY AGENTS:
 *   - Textualist scholars: institutional beneficiaries who control interpretive authority through monopoly on legitimate method
 *   - Rationalist jurists: marginalized payers who must either adopt textualist framing or operate outside mainstream institutional structures
 *   - Customary practice communities: powerless payers trapped between textual doctrine and operational necessity
 *   - Hanafi institutional authority: excluded agenda-setter with rival methodological framework and institutional power
 *   - Islamic jurisprudential tradition: observer seat representing historical pluralism and alternative reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__hanbali_reading, 0.68).
domain_priors:suppression_score(jurisprudential_method_kernel__hanbali_reading, 0.72).
domain_priors:theater_ratio(jurisprudential_method_kernel__hanbali_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__hanbali_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__hanbali_reading, "Hanbali Jurisprudential Method: Literal Text Primacy with Innovation Exclusion").
narrative_ontology:topic_domain(jurisprudential_method_kernel__hanbali_reading, "legal_philosophy/religious_authority/institutional_governance").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__hanbali_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__hanbali_reading, '2763649f-46db-4f54-b7ba-238620bea6f2').
narrative_ontology:cs_kernel_codification('2763649f-46db-4f54-b7ba-238620bea6f2', fixed_text).
narrative_ontology:cs_authority_grounding('2763649f-46db-4f54-b7ba-238620bea6f2', lineage).
narrative_ontology:cs_interpretation_layer_present('2763649f-46db-4f54-b7ba-238620bea6f2').
narrative_ontology:cs_reading_relation('2763649f-46db-4f54-b7ba-238620bea6f2', jurisprudential_method_kernel__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('2763649f-46db-4f54-b7ba-238620bea6f2', jurisprudential_method_kernel__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('2763649f-46db-4f54-b7ba-238620bea6f2', jurisprudential_method_kernel__shafii_reading, coexists_with).
narrative_ontology:cs_axiom('2763649f-46db-4f54-b7ba-238620bea6f2', foundational, qiyas_istihsan_are_forbidden_innovation).
narrative_ontology:cs_axiom_status(qiyas_istihsan_are_forbidden_innovation, holdable).
narrative_ontology:cs_axiom_grounding('2763649f-46db-4f54-b7ba-238620bea6f2', qiyas_istihsan_are_forbidden_innovation, deontological).
narrative_ontology:cs_axiom('2763649f-46db-4f54-b7ba-238620bea6f2', foundational, only_explicit_text_and_unanimous_consensus_are_valid_sources).
narrative_ontology:cs_axiom_status(only_explicit_text_and_unanimous_consensus_are_valid_sources, holdable).
narrative_ontology:cs_axiom_grounding('2763649f-46db-4f54-b7ba-238620bea6f2', only_explicit_text_and_unanimous_consensus_are_valid_sources, deontological).
narrative_ontology:cs_reference_frame('2763649f-46db-4f54-b7ba-238620bea6f2', prophetic_textual_authenticity).
narrative_ontology:cs_drift_state('2763649f-46db-4f54-b7ba-238620bea6f2', contemporary_institutional_enforcement_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2763649f-46db-4f54-b7ba-238620bea6f2', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__hanbali_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanbali_reading, textualist_scholars).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanbali_reading, conservative_institutions).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanbali_reading, rationalist_jurists).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanbali_reading, customary_practice_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Scholars who claim authority to interpret divine law through literal textual fidelity and reject analogical extension as corruption of the kernel. They control the interpretive hierarchy, gatekeep legitimate jurisprudential method, and derive authority and institutional position from the constraint's exclusion of rationalist tools. Their identity as 'preservers of the divine word' is constitutively fused with this methodological stance.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, textualist_scholars, beneficiary,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__hanbali_reading, textualist_scholars, agenda_setter).

% Jurists who use analogical reasoning (qiyas), juristic preference (istihsan), and rational extension to resolve cases not explicitly covered by text. The constraint delegitimizes their reasoning methods, excludes them from mainstream authority structures, and frames their work as bid'ah (forbidden innovation). They bear the cost of marginalization without controlling the institution that marginalizes them.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, rationalist_jurists, payer,
    organized, generational, constrained, global).

% Communities whose actual legal and social practices incorporate local custom, pragmatic adaptation, and rational extension beyond literal text. The constraint forces their practices into a binary: either they conform to literal textual interpretation (abandoning their situated knowledge) or their practices are declared bid'ah and illegitimate. They have no seat in the interpretive process and no exit from the jurisdictions governed by the constraint.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, customary_practice_communities, payer,
    powerless, biographical, trapped, regional).

% The Hanafi school and rationalist institutional frameworks that endorse extensive use of analogical reasoning and juristic preference. They are excluded from the constraint's definition of legitimate method; their authority structures are treated as corrupted by innovation. They would defend the necessity and divine legitimacy of rational extension but have no seat at the table defining 'legitimate kernel interpretation.'
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, hanafi_institutional_authority, excluded,
    institutional, civilizational, trapped, global).

% The Qur'an itself, insofar as the constraint claims to preserve its literal meaning against rationalist interpolation. This is a non-agent entity included for narrative completeness: the authority claim rests partly on defending 'the text itself' against distortion, though in practice only certain readings of the text are permitted.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, quranic_textual_corpus, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(jurisprudential_method_kernel__hanbali_reading, quranic_textual_corpus).

% The broader multi-school jurisprudential tradition that has historically accommodated multiple methodological approaches. This observer seat represents the tradition's capacity to recognize the constraint as ONE reading among legitimate alternatives, not as the sole canonical method.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, islamic_jurisprudential_tradition, observer,
    institutional, civilizational, analytical, global).

% Reform movements that argue for recourse to public interest (maslaha), rational adaptation to modern contexts, or epistemic diversity in jurisprudential method. The constraint excludes them from mainstream institutional authority by framing their methodological pluralism as corruption of the kernel. They would advocate for interpretive flexibility but are systematically delegitimized.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, contemporary_reform_movements, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jurisprudential_method_kernel__hanbali_reading, textualist_scholars).
narrative_ontology:fixing_cost_class(jurisprudential_method_kernel__hanbali_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified, verifiable basis for Islamic jurisprudence: if law comes only from explicit text and Companion consensus, disputes can (theoretically) be resolved by textual comparison rather than by competing rational arguments. The constraint coordinates by radically narrowing the legitimate interpretive toolkit, making disagreements about method questions that admit definitive textual answers.
% TRANSFER_FUNCTION: Transfers institutional authority from rationalist jurists and customary communities to textualist scholars: the power to define what counts as legitimate jurisprudential method becomes a monopoly held by those who control the text-based interpretive hierarchy. Those whose practices or reasoning styles fall outside the literal method lose authority and institutional position.
% ABSENT_VOICES: Rationalist jurists who would argue analogical reasoning is epistemically necessary for a living legal system are structurally excluded from the interpretive space; customary practice communities who would testify that literal interpretation is operationally insufficient for real disputes are systematically delegitimized; contemporary reform movements seeking methodological pluralism are locked out of mainstream authority.
% DISAPPEARANCE_RATIONALE: Textualist scholars and the institutions built on their authority would argue that if the constraint vanished, law would devolve into rationalist speculation and lose its foundation in divine revelation — the world would rearrange into spiritual chaos. Rationalist jurists and customary communities would argue that removing the constraint would restore jurisprudence's historical pluralism and allow legal systems to adapt to actual human situations — the world would become more workable. The contest is real: the constraint's disappearance WOULD reshape institutional authority, but whether that reshaping is rearrangement or restoration depends on the reading.
% FOUNDING_PROBLEM: In the early Islamic period, jurists' reasoning methods diverged: some extended divine law through analogy and preference, others claimed only explicit text was reliable; the risk was that law would fragment into competing schools with no common interpretive method, making unified Islamic governance impossible. The constraint was designed to resolve this by establishing textual literalism and Companion consensus as the non-negotiable kernel, excluding less-certain reasoning methods.
% FOUNDING_PROBLEM_CORROBORATION: Textualist scholars and conservative institutions attest the founding problem remains live: without rigid textual constraint, jurisprudence devolves into rationalist relativism. Rationalist jurists and legal historians attest the founding problem is partly obsolete: the pluralist multi-school tradition that developed DESPITE the constraint proved that jurisprudential diversity is sustainable and enriches legal discourse. Independent scholarship on the history of Islamic jurisprudence documents that all major schools, including Hanbali, eventually accommodated some degree of rational extension beyond literal text, suggesting the constraint was never as absolute as its formulation claimed.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__hanbali_reading, contested).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__hanbali_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__hanbali_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jurisprudential_method_kernel__hanbali_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jurisprudential_method_kernel__hanbali_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisprudential_method_kernel__hanbali_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jurisprudential_method_kernel__hanbali_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jurisprudential_method_kernel__hanbali_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.42 to 0.68 over the interval, reflecting intensifying institutional consolidation around the textualist reading. Early extractiveness (0.42) represents a period when multiple jurisprudential schools coexisted with more permeable boundaries; by interval end (0.68), the constraint's enforcement has hardened and rationalist alternatives are increasingly marginalized. Suppression requirement rises from 0.35 to 0.72, indicating that maintaining the constraint's exclusivity requires growing institutional effort: rationalist reasoning keeps arising in practice, requiring active pedagogical and institutional suppression to prevent its legitimation. Theater ratio climbs from 0.18 to 0.41, suggesting that an increasing fraction of enforcement activity consists of ritual gatekeeping (formal condemnations of bid'ah, pedagogical emphasis on textual purity) rather than substantive jurisprudential work. This is consistent with a constraint whose primary function (coordination through shared method) has been substantially accomplished, and whose persistence now depends on defending boundaries against intrusion. The measurements are authored on one shared grid; all metrics carry values at all time points.
 *
 * PERSPECTIVAL GAP:
 *   From the textualist agenda-setter's position, the constraint is genuine coordination: establishing a unified, verifiable textual method is necessary to prevent jurisprudential dissolution into competing rational schools. From this seat, extraction is minimal (only the cost of maintaining institutional discipline) and suppression is necessary defense of the kernel against corruption. From the rationalist jurist's position, the same structure operates as institutional exclusion: their legitimate reasoning methods are delegitimized, their authority is marginalized, and they are forced to either abandon their epistemology or operate outside mainstream institutions. From the customary community's position, the constraint is pure extraction with no coordination benefit: they are prevented from developing law responsive to their actual situations, with no seat in deciding what counts as legitimate method. The engine computes these divergent directionalities from the structural data: textualist scholars' directionality d is near 0.0 (beneficiary/low target), rationalist jurists' d is near 1.0 (high target), and customary communities' d is at the target end (trapped, identity_locked, powerless).
 *
 * DIRECTIONALITY LOGIC:
 *   Textualist scholars carry role=beneficiary (secondary: agenda_setter) with institutional power, civilizational horizon, and identity_locked exit — they are structurally incapable of exiting this constraint because their professional and intellectual identity is fused with the textualist method they defend. Their directionality d is near 0.0 (full beneficiary). Rationalist jurists carry role=payer with organized power and constrained exit: they have intellectual and methodological reasons to remain engaged with Islamic jurisprudence but are excluded from institutional authority and prestige. Their d is near 0.8 (high target). Customary practice communities carry role=payer with powerless and trapped exit: they have no choice but to accept the constraint's jurisdictional authority, and exiting means abandoning their cultural-legal identity. Their d is near 1.0 (full target). This directionality profile is the source of the measured extraction: the constraint extracts from those it targets (rationalists and customary communities) and subsidizes those it benefits (textualist scholars), with suppression as the enforcement machinery that maintains the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem — preventing jurisprudential fragmentation through a unified textual method — is partially solved by its operation. The multi-school Islamic tradition proves that jurisprudential diversity is compatible with institutional continuity, suggesting the founding problem is not as acute as the constraint's framing claims. However, the constraint itself has evolved: even Hanbali jurisprudence accommodated some degree of rational extension and analogical reasoning in practice, despite the official methodological claim. This creates a mandatrophy signal: the constraint's stated mandate (pure textual literalism with no analogical reasoning) has become disconnected from actual jurisprudential operation within Hanbali communities. The theater ratio's climb from 0.18 to 0.41 corroborates this: enforcement is increasingly theatrical (formal adherence to textual orthodoxy) rather than substantive (actually preventing rational reasoning in jurisprudential practice). The constraint persists not because it solves an urgent coordination problem, but because it legitimates institutional authority structures and distributes prestige/authority asymmetrically. This is the signature of a tangled_rope that has partially resolved its mandatrophy: the coordination function (shared method) is less critical than it was, but the extraction function (institutional authority distribution) is more entrenched.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textualism_operationality_gap,
    'Can pure literal textual interpretation actually resolve all novel jurisprudential cases that arise in a changing society, or does rationalist reasoning inevitably re-emerge even within communities formally committed to textual literalism?',
    'Historical analysis of actual Hanbali jurisprudential practice: did Hanbali scholars stay strictly to literal method, or did they eventually use rational extension, analogical reasoning, and contextual judgment despite the formal prohibition? Contemporary observation of Hanbali legal codes and fatwas: do they address novel questions (e.g., modern technology, contemporary social arrangements) through literal text alone, or through implicit analogical reasoning?',
    'If literal method proves operationally insufficient and rationalist reasoning re-emerges covertly, the constraint''s classification shifts: it becomes a snare whose coordination framing is cover for institutional extraction (theater_ratio rises). If literal method sustains itself as a living jurisprudential practice, the constraint retains its tangled_rope classification (coordination + extraction coexist). If the gap widens over time, T17 (mountain_extraction_accumulation) abductive triggers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textualism_operationality_gap, empirical, 'Whether formal textual literalism is operationally sustainable in jurisprudential practice.').

omega_variable(
    bid_ah_definition_contestation,
    'What makes a reasoning practice bid''ah (forbidden innovation) versus ijtihad (legitimate jurisprudential effort)? Is the distinction itself grounded in textual guidance or in the institutional power of textualist scholars to draw the line?',
    'Textual analysis: does the Qur''an or Hadith explicitly prohibit qiyas and istihsan, or does that prohibition rest on interpretive inference? Genealogical analysis: when and why did textualist scholars start categorizing rationalist reasoning as bid''ah? Did the category shift over time in response to institutional competition?',
    'If the bid''ah prohibition is an interpretive addition not grounded in explicit text, the constraint''s legitimacy rests on institutional power rather than divine guidance — classification shifts toward snare. If the prohibition IS textually explicit, the textualist reading has stronger grounding. The contradiction is real: if bid''ah is forbidden, and textual interpretation itself requires reasoned judgment (linguistic interpretation, hadith authentication), then textualist scholars are using forbidden practices to condemn forbidden practices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bid_ah_definition_contestation, conceptual, 'Whether bid''ah is a textual principle or an institutional categorization.').

omega_variable(
    customary_practice_suppression_mechanism,
    'Is the measured suppression (0.72) primarily structural (institutional exclusion, legal disability) or internalized (communities believe their practices are illegitimate, have fused their identity with textualist orthodoxy)?',
    'Post-exit dynamics: in communities or regions where textualist institutional enforcement weakens, do customary practices re-emerge rapidly and confident, or do they remain suppressed even after external enforcement is removed? Identity-fusion measurement: do customary practice communities frame their practices as ''wrong but necessary'' or as ''legitimate traditions that the authorities refuse to recognize''?',
    'If suppression is primarily structural, removing institutional enforcement would allow rapid practice-change and recovery of customary authority. If suppression is significantly internalized, the constraint''s effective suppression would persist even after institutional machinery is dismantled. This affects the cost of fixing the constraint: structural suppression is cheaper to reverse (change the rules) than internalized suppression (requires epistemic re-formation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_practice_suppression_mechanism, empirical, 'Structural vs. internalized suppression mechanism in customary practice communities.').

omega_variable(
    constraint_family_epistemological_competition,
    'Are the sibling readings (Hanafi, Maliki, Shafi''i, Hanbali) genuinely competing for the same institutional seat, or do they represent epistemologically distinct answers to the kernel that can coexist as legitimate alternatives?',
    'Historical analysis of coexistence: did these schools compete for monopoly or accommodate pluralism? Contemporary institutional mapping: in Muslim-majority jurisdictions today, do they coexist as recognized schools or is one institutionally dominant? Logical analysis: do their core axioms directly contradict (foreclosure) or reflect different weightings of the same sources (coexistence)?',
    'If readings are in foreclosure (one directly negates another), then the constraint describes a winner-take-all struggle for interpretive authority. If readings coexist, the constraint''s suppression of rationalist alternatives is not inevitable — institutional pluralism is sustainable. The measurement trajectory (theater_ratio climbing as extractiveness plateaus) suggests institutional consolidation around one reading rather than genuine epistemic competition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constraint_family_epistemological_competition, empirical, 'Whether jurisprudential schools are genuinely competing or can sustain pluralistic coexistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__hanbali_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(juri_tr_t0, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(juri_tr_t5, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 5, 0.21).
narrative_ontology:measurement(juri_tr_t10, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 10, 0.26).
narrative_ontology:measurement(juri_tr_t15, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 15, 0.31).
narrative_ontology:measurement(juri_tr_t25, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 25, 0.39).
narrative_ontology:measurement(juri_tr_t35, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 35, 0.41).
narrative_ontology:measurement(juri_tr_t40, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(juri_be_t0, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(juri_be_t5, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(juri_be_t10, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(juri_be_t15, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 15, 0.61).
narrative_ontology:measurement(juri_be_t25, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(juri_be_t35, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement(juri_be_t40, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(juri_su_t0, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(juri_su_t5, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 5, 0.42).
narrative_ontology:measurement(juri_su_t10, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 10, 0.51).
narrative_ontology:measurement(juri_su_t15, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 15, 0.59).
narrative_ontology:measurement(juri_su_t25, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 25, 0.68).
narrative_ontology:measurement(juri_su_t35, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 35, 0.72).
narrative_ontology:measurement(juri_su_t40, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__hanbali_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(jurisprudential_method_kernel__hanbali_reading, 0.12).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanbali_reading, jurisprudential_method_kernel__hanafi_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanbali_reading, jurisprudential_method_kernel__maliki_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanbali_reading, jurisprudential_method_kernel__shafii_reading).

% DUAL FORMULATION NOTE:
% This story is one reading of the contested jurisprudential_method_kernel. The Hanbali reading claims law derives from literal text and Companion consensus, with analogical reasoning rejected as bid'ah. This constraint (ε=0.68, tangled_rope, substantial extraction) competes with three sibling readings: the Hanafi reading (rationalist, extensive qiyas permitted), the Maliki reading (living Medinan practice as source), and the Shafi'i reading (strict four-tier hierarchy). These are NOT different perspectives on the same constraint; they are structurally distinct constraints with different ε values, beneficiary/victim structures, and classification profiles. They are linked because they all instantiate readings of the same kernel and their competition for institutional authority is the mechanism that sustains or transforms each constraint. Each story must be generated independently per the ε-invariance principle; this story generates only the Hanbali reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jurisprudential_method_kernel__hanbali_reading, organized, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
