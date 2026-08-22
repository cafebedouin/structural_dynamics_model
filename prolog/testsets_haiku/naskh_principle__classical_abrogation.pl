% ============================================================================
% CONSTRAINT STORY: naskh_principle__classical_abrogation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_naskh_principle__classical_abrogation, []).

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
 *   constraint_id: naskh_principle__classical_abrogation
 *   human_readable: Classical Naskh Principle: Chronological Quranic Abrogation
 *   domain: theological/jurisprudential
 *
 * SUMMARY:
 *   The classical naskh principle (abrogation) is a foundational doctrine in
 *   Islamic jurisprudence asserting that later-revealed Quranic verses
 *   supersede earlier verses on the same legal topic, based on chronological
 *   order of revelation. This constraint instantiates ONE reading of the
 *   contested kernel 'naskh_principle'—specifically, the classical_abrogation
 *   reading that treats naskh as a clear, hierarchical,
 *   chronologically-ordered supersession mechanism. The reading benefits the
 *   classical Islamic legal tradition by providing interpretive certainty and
 *   institutional stability, and victimizes interpretive flexibility and
 *   theological coherence-seeking by marginalizing alternative readings. The
 *   constraint is simultaneously claimed as Tangled Rope (coordination +
 *   asymmetric extraction) and exhibits measurably high suppression (0.71),
 *   indicating that the classical framework actively excludes competing
 *   hermeneutic voices. This is one story in a three-story constraint family:
 *   it coexists with contextual_harmonization (which reads all verses as
 *   valid in their contexts) and influences progressive_restriction (which
 *   reads the movement as pedagogical rather than supersessional). The
 *   measurement interval spans 1400 years from the Prophet Muhammad's
 *   ministry to present-day jurisprudential practice, capturing the
 *   codification and institutionalization of naskh across the classical
 *   schools.
 *
 * KEY AGENTS:
 *   - Classical Islamic jurists (Hanafi, Maliki, Shafi'i, Hanbali schools): institutional agenda-setters who codified naskh as the authoritative interpretive method; hold institutional authority over the naskh canon; power = institutional; exit = identity-locked.
 *   - Legal certainty framework (non-agent): the doctrinal commitment to fixed, knowable legal rules; benefits from naskh by providing a supersession algorithm; power = analytical.
 *   - Contextual interpretation tradition: scholars emphasizing contextual, situational, or literary-unity readings; bear the cost of interpretive marginalization; power = moderate; exit = constrained.
 *   - Theological coherence seekers: theologians and Quranic scholars seeking holistic meaning; constrained from interpretations that honor all verses equally; power = organized; exit = constrained.
 *   - Contemporary reform schools: progressive Islamic scholars and modernist jurists; excluded from classical authority structure; power = moderate; exit = mobile.
 *   - Islamic legal tradition (non-agent): institutional beneficiary; benefits from naskh's provision of meta-rule for handling textual conflict; power = institutional.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(naskh_principle__classical_abrogation, 0.68).
domain_priors:suppression_score(naskh_principle__classical_abrogation, 0.71).
domain_priors:theater_ratio(naskh_principle__classical_abrogation, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, extractiveness, 0.68).
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(naskh_principle__classical_abrogation, tangled_rope).
narrative_ontology:human_readable(naskh_principle__classical_abrogation, "Classical Naskh Principle: Chronological Quranic Abrogation").
narrative_ontology:topic_domain(naskh_principle__classical_abrogation, "theological/jurisprudential").

domain_priors:requires_active_enforcement(naskh_principle__classical_abrogation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(naskh_principle__classical_abrogation, '65b6f59d-c65a-4c32-9fbb-d984206952af').
narrative_ontology:cs_kernel_codification('65b6f59d-c65a-4c32-9fbb-d984206952af', fixed_text).
narrative_ontology:cs_authority_grounding('65b6f59d-c65a-4c32-9fbb-d984206952af', lineage).
narrative_ontology:cs_interpretation_layer_present('65b6f59d-c65a-4c32-9fbb-d984206952af').
narrative_ontology:cs_reading_relation('65b6f59d-c65a-4c32-9fbb-d984206952af', naskh_principle__contextual_harmonization, coexists_with).
narrative_ontology:cs_reading_relation('65b6f59d-c65a-4c32-9fbb-d984206952af', naskh_principle__progressive_restriction, influences).
narrative_ontology:cs_axiom('65b6f59d-c65a-4c32-9fbb-d984206952af', foundational, later_revelation_chronologically_supersedes_earlier).
narrative_ontology:cs_axiom_status(later_revelation_chronologically_supersedes_earlier, holdable).
narrative_ontology:cs_axiom_grounding('65b6f59d-c65a-4c32-9fbb-d984206952af', later_revelation_chronologically_supersedes_earlier, deontological).
narrative_ontology:cs_axiom('65b6f59d-c65a-4c32-9fbb-d984206952af', foundational, abrogated_verses_lose_legal_bindingness).
narrative_ontology:cs_axiom_status(abrogated_verses_lose_legal_bindingness, holdable).
narrative_ontology:cs_axiom_grounding('65b6f59d-c65a-4c32-9fbb-d984206952af', abrogated_verses_lose_legal_bindingness, conventional).
narrative_ontology:cs_reference_frame('65b6f59d-c65a-4c32-9fbb-d984206952af', chronological_supersession_hierarchy).
narrative_ontology:cs_drift_state('65b6f59d-c65a-4c32-9fbb-d984206952af', contemporary_jurisprudential_contestation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('65b6f59d-c65a-4c32-9fbb-d984206952af', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(naskh_principle__classical_abrogation, naskh_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(naskh_principle__classical_abrogation, classical_jurists).
narrative_ontology:constraint_beneficiary(naskh_principle__classical_abrogation, legal_certainty_framework).
narrative_ontology:constraint_victim(naskh_principle__classical_abrogation, contextual_interpretation_tradition).
narrative_ontology:constraint_victim(naskh_principle__classical_abrogation, theological_coherence_seekers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Classical Islamic legal schools (Hanafi, Maliki, Shafi'i, Hanbali) codified naskh as the authoritative interpretive method for resolving apparent Quranic contradictions. They set the canon of which verses abrogate which, establish the rules for identifying abrogation (later revelation chronology, clear textual markers, juristic consensus), and maintain the institutional authority to adjudicate abrogation claims. Their professional identity and scholarly reputation are invested in the stability and coherence of the classical interpretive taxonomy. They collect interpretive authority and institutional prestige from maintaining this system.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, classical_jurists, agenda_setter,
    institutional, generational, identity_locked, universal).

% A non-agent entity: the institutional commitment to fixed, knowable legal rules with a clear hierarchy of authority. The naskh principle serves this commitment by providing an algorithm for resolving textual conflict: later text supersedes earlier. This framework benefits from the constraint because it guarantees legal predictability and institutional stability.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, legal_certainty_framework, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(naskh_principle__classical_abrogation, legal_certainty_framework).

% Scholars and schools emphasizing contextual reading (including contemporary Quranic hermeneutics and some classical juristic traditions like Maliki jurisprudence's local practice emphasis) pay the cost of the naskh constraint by losing interpretive space. Under classical naskh, verses they read as contextually valid in their original revelation setting are declared legally inoperative. Their interpretive frameworks are marginalized as less rigorous or less authoritative than the chronological supersession model. They bear the burden of demonstrating why contextual reading is not incoherent relativism.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, contextual_interpretation_tradition, payer,
    moderate, generational, constrained, universal).

% Theologians and Quranic scholars seeking to understand the Quran as a coherent whole face a cost: the naskh principle allows them to dismiss apparent contradictions as simply 'abrogated verses' rather than engaging the theological tension. This can foreclose deeper analysis of divine wisdom, progressive moral teaching, or textual unity. They are constrained from pursuing interpretations that honor all verses equally because the institutional framework has already declared some verses legally void.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, theological_coherence_seekers, payer,
    organized, generational, constrained, universal).

% Contemporary Islamic reform movements, modernist scholars, and progressive juristic voices are structurally excluded from the core interpretive authority that defines what counts as 'classical' and therefore authoritative. They can offer alternative readings (contextual harmonization, progressive restriction), but they do so outside the classical institutional framework and without the centuries-long institutional weight. Their exclusion is what the classical authority structure maintains—recognition of their hermeneutic legitimacy would require admitting that the classical naskh canon is contestable.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, contemporary_reform_schools, excluded,
    moderate, biographical, mobile, national).

% The Quranic text is the constraint's object and reference point, not an agent. It is included analytically because the interpretation system (naskh) is literally about what the text means and which parts of it remain legally operative. The text does not act but is acted upon by the interpretive system.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, revelation_text_itself, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(naskh_principle__classical_abrogation, revelation_text_itself).

% The broader Islamic legal tradition as an institutional form benefits from naskh by having a stable, systematized framework for jurisprudential operation. The tradition can grow, elaborate, and refine rules without internal collapse because naskh provides the meta-rule for handling textual conflict. The constraint is what makes the tradition coherent as an institution.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, islamic_legal_tradition, beneficiary,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(naskh_principle__classical_abrogation, islamic_legal_tradition).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(naskh_principle__classical_abrogation, classical_jurists).
narrative_ontology:fixing_cost_class(naskh_principle__classical_abrogation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves apparent contradictions in the Quranic text by establishing a chronological supersession hierarchy: where two verses on the same legal topic appear to conflict, the verse revealed later in the Prophet Muhammad's ministry legally supersedes the earlier one. This provides a decision procedure for juristic reasoning that does not require dismissing parts of the text as inauthentic, corrupt, or metaphorical—instead, both are authentic but one is no longer legally operative. The coordination problem solved is: how can a divinely revealed text that contains apparent legal contradictions be a coherent source of law?
% TRANSFER_FUNCTION: Transfers interpretive authority from contextual or holistic reading of the text toward chronological-historical reading. It moves the locus of meaning-making from 'what does this verse mean in its context?' toward 'when was this revealed relative to other verses on this topic?' Classical jurists who master the chronology of revelation and the established abrogation canon gain authority over scholars who emphasize contextual or theological coherence. It also transfers legal force: abrogated verses retain historical and spiritual significance but lose legal bindingness, which is transferred to their replacements.
% ABSENT_VOICES: Scholars emphasizing the contextual, situational, or progressive-pedagogical reading of Quranic revelation are structurally excluded from the core definitional authority. Contemporary Quranic hermeneutics emphasizing literary unity, thematic coherence, or multi-dimensional meaning are marginalized as less rigorous than classical juristic methodology. Communities that practice Islamic law outside the classical school framework (folk Islam, local practice traditions, reformist movements) are not represented in the authority that sets the naskh canon. Women scholars and voices outside the male-dominated classical juristic circles were absent from the original codification and remain underrepresented in its authority structure.
% DISAPPEARANCE_RATIONALE: If the classical naskh principle and its canonical taxonomy disappeared overnight, Islamic jurisprudence would undergo fundamental restructuring. Legal certainty would collapse temporarily: jurists would face every apparent Quranic contradiction fresh, without the ready algorithm 'later abrogates earlier.' The classical four schools would have to justify their jurisprudential positions without reference to the naskh canon—some positions would lose their textual anchor. Contemporary reform movements, contextual hermeneutics, and non-classical interpretive schools would immediately gain institutional space and authority. The Quranic text would need to be re-engaged holistically, which would either produce new synthetic jurisprudence or fragment into competing schools with different resolutions of apparent contradictions. The institutional stability the classical framework provides would be disrupted, and the Islamic legal tradition would reorganize around a different hermeneutic principle (contextual, progressive, or pluralistic).
% FOUNDING_PROBLEM: In the first centuries of Islam, Islamic jurisprudence faced a foundational crisis: the Quranic text contained passages that appeared to contradict each other on matters of law and practice. Early jurists (7th–9th centuries) observed verses on war, dietary rules, marriage, inheritance, and other topics that seemed to conflict. The founding problem was: How can a revealed text that is believed to be divinely preserved be internally contradictory? The naskh principle (abrogation) solved this by proposing that later revelation superseded earlier on the same topic—preserving the integrity of the text by declaring that both revelations were authentic but temporally ordered. This allowed jurisprudence to proceed on the assumption of textual coherence without requiring emendation or rejection of any verse.
% FOUNDING_PROBLEM_CORROBORATION: Classical Islamic scholars from the 9th century onward (al-Shafi'i, al-Tabari, al-Razi) attested that the founding problem was live and pressing, and that naskh was the solution they adopted. However, contemporary Quranic scholars and hermeneuticists—notably outside the classical juristic schools—attest that the founding problem is either resolved differently or overstated. They point to the Quranic text itself containing signals of context-dependency (verse 2:106 on abrogation, which classical scholars cite, is itself contested in meaning) and argue that the founding problem can be solved through contextual reading without requiring chronological supersession. Theologians like Fazlur Rahman and contemporary reform scholars attest that the classical naskh canon has been used to suppress interpretive voices and that it is not the only coherent solution. The founding problem's status is contested between the classical juristic establishment (which maintains it is live and naskh is necessary) and contemporary revisionist traditions (which argue it is either dead or misdiagnosed).
narrative_ontology:disappearance_verdict(naskh_principle__classical_abrogation, world_rearranges).
narrative_ontology:founding_problem_status(naskh_principle__classical_abrogation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(naskh_principle__classical_abrogation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(naskh_principle__classical_abrogation, 'none', 1).
narrative_ontology:epsilon_provenance(naskh_principle__classical_abrogation, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(naskh_principle__classical_abrogation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(naskh_principle__classical_abrogation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(naskh_principle__classical_abrogation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.68 at interval end) because the classical naskh principle consolidates interpretive authority in the hands of classical jurists and their institutional descendants, marginalizing other hermeneutic voices and imposing a specific reading strategy on all Quranic interpretation. Suppression is high (0.71) because the constraint actively enforces the exclusion of contextual and coherence-seeking readings: they are not merely disfavored but are defined as 'not rigorous' or 'not classical.' Theater ratio rises over time (0.25 at t=0 to 0.42 at t=1400), suggesting an increasing gap between the justification (resolving textual contradiction) and the function (maintaining institutional control). At early time points (t=0-200), naskh genuinely solves a coordination problem (jurisprudence had nowhere else to turn); by the classical period (t=400+), it becomes more a mechanism for institutional stability and less about solving textual problems (since the canon is now fixed and well-established). Accessibility collapse is moderate-high (0.72): once one learns chronological revelation order and the classical naskh taxonomy, alternatives (reading all verses as contextually valid) become harder to see—the framework colonizes interpretive possibility space. Resistance is moderate (0.58): contextual and progressive readings persist and find modern audiences, even if marginalized institutionally. The measurement series captures the transition from naskh as an emergent problem-solving principle to naskh as an institutionalized and enforced orthodoxy.
 *
 * PERSPECTIVAL GAP:
 *   The classical jurist seat and the contextual-tradition seat should compute vastly differently from this constraint. From the jurist's position, naskh is genuine coordination: it makes jurisprudence possible by resolving textual conflict and allowing stable law. From the contextual reader's position, the same structure operates as suppressive institutional control: their interpretive frameworks are defined as inferior before being examined. The jurist experiences low extraction (they benefit from institutional stability and interpretive authority) and high accessibility collapse (alternative readings seem incoherent from inside the classical framework). The contextual reader experiences high extraction (their interpretive space is foreclosed) and the same high accessibility collapse (the classical framework is so institutionally entrenched that questioning it seems naive). The engine should compute these divergences from the structural data: jurists get low directionality (beneficiary seat, high power, identity-locked exit, institutional scope) and contextual readers get high directionality (payer seat, moderate power, constrained exit, universal scope). This per-seat divergence is what Tangled Rope structures: genuine coordination and asymmetric extraction occurring through the same mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Classical jurists benefit from naskh by holding institutional authority over its application. They are the canonical interpreters, their status derives from mastery of the revelation chronology and abrogation taxonomy, and their institutional power is premised on this framework remaining legitimate. Exit for classical jurists is identity-locked: to leave this framework means abandoning their professional identity, their centuries of institutional authority, and the legitimacy of their jurisprudential schools. Directionality for classical jurists approaches 0.0 (full beneficiary). Contextual interpretation traditions and theological coherence seekers are targets: their interpretive space is systematically foreclosed by the classical framework, and exit is constrained—they can develop alternative readings but do so in the margins of Islamic jurisprudence, without institutional weight. Directionality for these seats approaches 1.0 (full target). Legal certainty framework and the Islamic legal tradition are non-agents but benefit from the coordination function naskh provides. The constraint's spatial scope is universal: naskh operates across all Islamic traditions and geographic regions where the classical schools hold authority. The time horizon for jurists is generational: institutional authority passes through scholarly lineages and institutional inheritance. For theological coherence seekers it is also generational, but they lack institutional power to transmit their alternative framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (resolving Quranic textual contradictions) was genuinely alive in the first two centuries of Islamic jurisprudence (t=0-200). Naskh solved a real coordination problem: jurisprudence had no other framework for handling apparent contradiction. By the classical period (t=400+), however, the founding problem's status shifts. The classical naskh canon is now fixed and well-established; jurisprudence no longer struggles to resolve contradictions because the classical schools have already done this work. The question is no longer 'How do we resolve contradiction?' but 'Who has the authority to interpret the naskh canon?' This represents a classic mandatrophy situation: the mandate (solve textual contradiction via chronological ordering) has outlived its primary function (the problem is solved and encoded), but the constraint persists because it now serves secondary functions (institutional authority, legal certainty framework maintenance, exclusion of alternative hermeneutics). The theater ratio rising from 0.25 to 0.42 captures this: early on, naskh is functional problem-solving; later, it becomes increasingly performative (scholars debate which verses abrogate which, not because textual contradiction is live but because the debate itself maintains institutional authority). The measurement interval shows the lifecycle: early functional phase (t=0-200, theater low), transition to institutional phase (t=200-700, theater rising), mature institutional phase (t=700-1400, theater flattens at ~0.41-0.42, indicating stable theatrical maintenance). The contradiction between high extraction (0.68) and the suppressed founding problem suggests that classical jurists and the legal tradition benefit from maintaining naskh's authority precisely because it is no longer functionally necessary—the extraction it provides is no longer justified by solving the original coordination problem. This is the mandatrophy verdict: the constraint has outlived its mandate and now persists through institutional inertia and exclusion of alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    chronology_of_revelation_certainty,
    'Is the chronological ordering of Quranic revelation historically and textually certain enough to serve as the basis for a legal supersession hierarchy?',
    'Historical-critical analysis of the Quranic text, hadith literature, and early Islamic sources; contemporary Quranic chronology scholarship (Nöldeke, Blachère, Watt, contemporary Islamic and Western scholars) examining the evidentiary basis for verse-by-verse dating.',
    'If chronology is substantially uncertain (significant verses or passages cannot be reliably dated relative to others), then the entire naskh principle loses epistemic grounding. Legal rulings based on ''later revelation'' would rest on historically speculative grounds. This would support contextual_harmonization or progressive_restriction readings, which do not depend on precise chronology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chronology_of_revelation_certainty, empirical, 'Whether the chronological ordering of Quranic revelation is certain enough to ground a legal supersession principle.').

omega_variable(
    separation_of_coordination_and_extraction,
    'Are the coordination function (resolving textual contradiction) and the extraction function (consolidating interpretive authority) structurally separable in the naskh principle?',
    'Conceptual analysis: could a jurisprudential system adopt the problem-solving aspects of naskh (a decision procedure for handling textual conflict) without adopting its institutional gate-keeping? Could contextual or progressive readings provide the same legal certainty without the same authority consolidation?',
    'If separable, the constraint is better classified as Snare (pure extraction of interpretive authority with a coordination cover story) than Tangled Rope (genuine coordination plus extraction). If inseparable, then high suppression and extraction are intrinsic to solving the founding coordination problem—the trade-off between certainty and flexibility is unavoidable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(separation_of_coordination_and_extraction, conceptual, 'Whether naskh''s coordination and extraction functions are structurally inseparable.').

omega_variable(
    textual_incoherence_reality,
    'Does the Quranic text genuinely contain legal contradictions that require resolution, or are apparent contradictions artifacts of reading-strategy and can be harmonized without chronological supersession?',
    'Detailed Quranic hermeneutics: contextual analysis of allegedly contradictory verses (e.g., verses on warfare, alcohol, inheritance); examination of whether contextual, conditional, or situational readings can harmonize them; assessment by contemporary Quranic scholars using literary and theological methods.',
    'If apparent contradictions are harmonizable through context, the founding problem is partially misdiagnosed and naskh is an over-solution. Contextual_harmonization would then be the more parsimonious reading. If contradictions are real and irreducible, naskh''s problem-solving function is more robustly justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_incoherence_reality, conceptual, 'Whether apparent Quranic textual contradictions are real or artifacts of reading-strategy.').

omega_variable(
    kernel_definition_contestation,
    'Does the Quranic text itself settle whether naskh (chronological supersession) is the intended interpretive method, or is the method itself a human juristic invention imposed onto the text?',
    'Textual examination: analysis of Quranic verses discussing abrogation (notably 2:106, 16:101) and their own interpretation in classical and contemporary scholarship; assessment of whether these verses endorse chronological supersession or allow alternative readings.',
    'If the text itself is ambiguous or does not clearly endorse naskh, then the principle is a human-authored reading choice, not a textual fact—which strengthens the contextual_harmonization and progressive_restriction readings as equally defensible. If the text clearly endorses chronological supersession, classical_abrogation is more firmly grounded.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_definition_contestation, conceptual, 'Whether the Quranic text itself settles the naskh question or leaves it interpretively open.').

omega_variable(
    institutional_identity_lock_mechanism,
    'To what extent is the perpetuation of naskh driven by institutional path-dependence and scholarly identity-fusion, versus by genuine problem-solving necessity?',
    'Institutional and sociological analysis: historical examination of how naskh became canonical (when exactly was it institutionalized across all four schools), how juristic careers became dependent on mastery of the naskh taxonomy, how alternative readings were marginalized over time, and what would happen to jurists'' authority if naskh were abandoned.',
    'If naskh persists primarily through institutional identity-lock (scholars cannot leave without abandoning their status, schools cannot revise without institutional collapse), then it meets the Piton profile (degraded function maintained through institutional inertia and theater) more than Tangled Rope. High identity-lock supports the theater-ratio interpretation: the constraint is increasingly performative rather than functionally necessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_identity_lock_mechanism, empirical, 'Whether naskh persists through institutional identity-lock more than through genuine problem-solving necessity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(naskh_principle__classical_abrogation, 0, 1400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nask_tr_t0, naskh_principle__classical_abrogation, theater_ratio, 0, 0.25).
narrative_ontology:measurement(nask_tr_t200, naskh_principle__classical_abrogation, theater_ratio, 200, 0.32).
narrative_ontology:measurement(nask_tr_t400, naskh_principle__classical_abrogation, theater_ratio, 400, 0.38).
narrative_ontology:measurement(nask_tr_t700, naskh_principle__classical_abrogation, theater_ratio, 700, 0.4).
narrative_ontology:measurement(nask_tr_t1000, naskh_principle__classical_abrogation, theater_ratio, 1000, 0.41).
narrative_ontology:measurement(nask_tr_t1400, naskh_principle__classical_abrogation, theater_ratio, 1400, 0.42).

% Extraction over time
narrative_ontology:measurement(nask_be_t0, naskh_principle__classical_abrogation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(nask_be_t200, naskh_principle__classical_abrogation, base_extractiveness, 200, 0.52).
narrative_ontology:measurement(nask_be_t400, naskh_principle__classical_abrogation, base_extractiveness, 400, 0.61).
narrative_ontology:measurement(nask_be_t700, naskh_principle__classical_abrogation, base_extractiveness, 700, 0.65).
narrative_ontology:measurement(nask_be_t1000, naskh_principle__classical_abrogation, base_extractiveness, 1000, 0.67).
narrative_ontology:measurement(nask_be_t1400, naskh_principle__classical_abrogation, base_extractiveness, 1400, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(nask_su_t0, naskh_principle__classical_abrogation, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(nask_su_t200, naskh_principle__classical_abrogation, suppression_requirement, 200, 0.58).
narrative_ontology:measurement(nask_su_t400, naskh_principle__classical_abrogation, suppression_requirement, 400, 0.64).
narrative_ontology:measurement(nask_su_t700, naskh_principle__classical_abrogation, suppression_requirement, 700, 0.68).
narrative_ontology:measurement(nask_su_t1000, naskh_principle__classical_abrogation, suppression_requirement, 1000, 0.7).
narrative_ontology:measurement(nask_su_t1400, naskh_principle__classical_abrogation, suppression_requirement, 1400, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(naskh_principle__classical_abrogation, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(naskh_principle__classical_abrogation, 0.18).
narrative_ontology:affects_constraint(naskh_principle__classical_abrogation, naskh_principle__contextual_harmonization).
narrative_ontology:affects_constraint(naskh_principle__classical_abrogation, naskh_principle__progressive_restriction).

% DUAL FORMULATION NOTE:
% This constraint is part of the naskh_principle kernel family with three structurally distinct readings. The classical_abrogation reading (this story) treats naskh as chronological supersession and exhibits high extraction despite genuine coordination benefits. The contextual_harmonization reading (separate constraint story) asserts all verses remain valid within context and produces a lower-extraction structure. The progressive_restriction reading (separate constraint story) reads the movement as pedagogical rather than invalidating and produces yet different victim/beneficiary distribution. The three stories are linked by network.affects_constraints to enable comparative analysis of how different readings of the same kernel produce different constraint classifications. Each reading has its own ε (classical_abrogation=0.68, contextual_harmonization lower, progressive_restriction intermediate), its own beneficiary/victim structure, and its own stakeholder situational descriptions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(naskh_principle__classical_abrogation, moderate, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
