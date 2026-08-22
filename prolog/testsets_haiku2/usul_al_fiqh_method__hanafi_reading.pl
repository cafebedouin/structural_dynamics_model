% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__hanafi_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usul_al_fiqh_method__hanafi_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: usul_al_fiqh_method__hanafi_reading
 *   human_readable: Hanafi Methodological Framework for Islamic Jurisprudence
 *   domain: legal/theological
 *
 * SUMMARY:
 *   The Hanafi school of Islamic jurisprudence developed a distinctive
 *   methodological framework that grants broad scope for analogical reasoning
 *   (qiyas), supplemental reasoned opinion (ra'y), and juristic preference
 *   (istihsan) when textual sources are silent or when strict analogy
 *   produces unjust outcomes. This framework represents one reading of a
 *   contested kernel — the proper method for deriving Islamic law from
 *   foundational sources (Quran, hadith, ijma, qiyas). The Hanafi reading
 *   prioritizes the rationalist jurist's capacity to extend law through
 *   analogy and to override analogy when public interest demands it, thereby
 *   maximizing methodological flexibility and the interpretive authority of
 *   trained scholars. This reading coexists with more restrictive
 *   alternatives: the Hanbali school minimizes qiyas and prioritizes textual
 *   fidelity; the Maliki school grounds law in Medinan practice and
 *   unrestricted public interest; the Shafii school systematizes all sources
 *   under a strict hierarchy with hadith authentication as prerequisite. The
 *   constraint described here is the Hanafi instantiation — the specific
 *   structural arrangement that results from adopting this methodological
 *   reading. Its extractiveness is moderate-high (0.68) because it
 *   systematically privileges expert jurist reasoning over lay textual
 *   understanding and because istihsan permits departure from analogy based
 *   on jurist judgment of public interest, which creates both coordination
 *   capacity and potential for arbitrary authority concentration. Suppression
 *   is moderate (0.52) because the framework is maintained through
 *   institutional gatekeeping (curriculum, appointments, legal codification)
 *   rather than coercive external force — the suppression is embedded in the
 *   credentialing system itself.
 *
 * KEY AGENTS:
 *   - Hanafi jurists: institutional agenda-setters and primary beneficiaries; control the methodological apparatus and define acceptable reasoning moves
 *   - Rational methodology scholars: institutional beneficiaries; validate their expertise through the framework's institutional dominance
 *   - Textualist and literalist jurists: moderate-power payersonstrained by institutional dominance; forced to operate within a discursive field they contest
 *   - Non-rational legal claimants: powerless payerstrappedin a system requiring expert credentials they cannot easily obtain
 *   - Maliki and Hanbali jurists: excluded institutional competitors; maintain regional authority but lack the dominance Hanafi institutional entrenchment provides
 *   - Ottoman state apparatus: institutional agenda-setter and secondary beneficiary; adopted Hanafi framework as official method and benefited from its systematicity
 *   - Lay communities: dual-positioned; benefit from clear, determinate legal guidance but bear costs when istihsan produces rules diverging from lay understanding
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__hanafi_reading, 0.68).
domain_priors:suppression_score(usul_al_fiqh_method__hanafi_reading, 0.52).
domain_priors:theater_ratio(usul_al_fiqh_method__hanafi_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__hanafi_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__hanafi_reading, "Hanafi Methodological Framework for Islamic Jurisprudence").
narrative_ontology:topic_domain(usul_al_fiqh_method__hanafi_reading, "legal/theological").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__hanafi_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__hanafi_reading, '3d55bc0d-51be-4cfe-ae1c-79b2898aaac6').
narrative_ontology:cs_kernel_codification('3d55bc0d-51be-4cfe-ae1c-79b2898aaac6', fixed_text).
narrative_ontology:cs_authority_grounding('3d55bc0d-51be-4cfe-ae1c-79b2898aaac6', extraction).
narrative_ontology:cs_interpretation_layer_present('3d55bc0d-51be-4cfe-ae1c-79b2898aaac6').
narrative_ontology:cs_reading_relation('3d55bc0d-51be-4cfe-ae1c-79b2898aaac6', usul_al_fiqh_method__hanbali_reading, coexists_with).
narrative_ontology:cs_reading_relation('3d55bc0d-51be-4cfe-ae1c-79b2898aaac6', usul_al_fiqh_method__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('3d55bc0d-51be-4cfe-ae1c-79b2898aaac6', usul_al_fiqh_method__shafii_reading, influences).
narrative_ontology:cs_axiom('3d55bc0d-51be-4cfe-ae1c-79b2898aaac6', foundational, qiyas_expansion_when_sources_silent).
narrative_ontology:cs_axiom_status(qiyas_expansion_when_sources_silent, holdable).
narrative_ontology:cs_axiom_grounding('3d55bc0d-51be-4cfe-ae1c-79b2898aaac6', qiyas_expansion_when_sources_silent, deontological).
narrative_ontology:cs_axiom('3d55bc0d-51be-4cfe-ae1c-79b2898aaac6', foundational, istihsan_public_interest_override).
narrative_ontology:cs_axiom_status(istihsan_public_interest_override, holdable).
narrative_ontology:cs_axiom_grounding('3d55bc0d-51be-4cfe-ae1c-79b2898aaac6', istihsan_public_interest_override, instrumental).
narrative_ontology:cs_reference_frame('3d55bc0d-51be-4cfe-ae1c-79b2898aaac6', rationalist_jurist_authority).
narrative_ontology:cs_drift_state('3d55bc0d-51be-4cfe-ae1c-79b2898aaac6', contemporary_legal_pluralism_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3d55bc0d-51be-4cfe-ae1c-79b2898aaac6', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanafi_reading, hanafi_jurists).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanafi_reading, rational_methodology_scholars).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanafi_reading, textualist_literalist_jurists).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanafi_reading, non_rational_legal_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanafi_reading, lay_communities).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanafi_reading, ottoman_state_apparatus).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanafi_reading, lay_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control the interpretive apparatus by which Islamic law is derived from foundational sources. They set the canon of acceptable methodological moves: qiyas expansion, ra'y supplementation, istihsan deployment. Their authority depends on maintaining the framework's legitimacy as a rational, systematic method superior to mere textualism. They benefit directly from the framework's acceptance because it validates their professional expertise and preserves their interpretive gatekeeping power.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, hanafi_jurists, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Scholars who ground their authority in epistemological sophistication — the claim that rigorous analogical and rationalist methods yield more coherent law than literalism. They benefit from the Hanafi framework's institutional dominance because it validates their intellectual commitments and attracts students and patronage to rationalist schools of thought.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, rational_methodology_scholars, beneficiary,
    institutional, civilizational, mobile, global).

% Maintain an alternative jurisprudential method emphasizing textual fidelity and minimizing analogical extension. They must operate in a discursive environment where the Hanafi framework controls institutional legitimacy, funding, and appointment to teaching positions. Their exit — establishing a competing methodological monopoly in a new territory — is constrained by the Hanafi framework's institutional entrenchment. They pay in reduced institutional access and diminished persuasive authority over time.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, textualist_literalist_jurists, payer,
    moderate, civilizational, constrained, global).

% Communities, lay jurists, and folk practitioners who lack the training in rationalist methodology to participate in the high-level jurisprudential game. They cannot challenge the framework because they lack the epistemic credentials; they are trapped within a legal system whose authoritative rulings are inaccessible without sponsorship from the credentialed class. They bear the cost of innovations introduced via istihsan that may not reflect their understanding or consent.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, non_rational_legal_claimants, payer,
    powerless, biographical, trapped, global).

% Maintain a competing method grounded in Medinan practice and unrestricted public interest (maslaha mursala). They are structurally excluded from the Hanafi institutional dominance in the Ottoman and post-Ottoman spheres, though they retain regional authority. They would argue that the Hanafi framework's rationalism privileges jurist speculation over grounded communal practice.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, maliki_jurists, excluded,
    institutional, civilizational, constrained, regional).

% Maintain the most restrictive methodological stance, emphasizing textual maximization and qiyas minimization. They are excluded from mainstream institutional authority but retain scholarly credibility. They would argue the Hanafi framework permits unjustified innovation and usurps authority from the primary sources.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, hanbali_textualists, excluded,
    moderate, civilizational, constrained, regional).

% Developed systematic usul al-fiqh as a meta-discipline, establishing source hierarchy that constrains but permits Hanafi-style reasoning. They observe the Hanafi framework as one instance of a broader jurisprudential landscape. They occupy an analytical seat, able to compare methodological efficiency and coherence without depending on any single framework's dominance.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, shafii_systematizers, observer,
    institutional, civilizational, mobile, global).

% Receive clear, systematic legal guidance on matters of daily life — the Hanafi framework does produce determinate rules. They also bear the cost of rules derived via rationalist istihsan that may diverge from their own understanding of Islamic principle or from the plain textual reading they might expect.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, lay_communities, beneficiary,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__hanafi_reading, lay_communities, payer).

% Adopted the Hanafi framework as the official jurisprudential method and consolidated its institutional dominance through administrative appointments, curriculum control, and legal codification. The framework served the state's interest in uniform, governable law that could be applied across diverse territories. The state benefited from the framework's rationalist systematicity while maintaining theological legitimacy through its grounding in Islamic sources.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, ottoman_state_apparatus, agenda_setter,
    institutional, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__hanafi_reading, ottoman_state_apparatus, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(usul_al_fiqh_method__hanafi_reading, hanafi_jurists).
narrative_ontology:fixing_cost_class(usul_al_fiqh_method__hanafi_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a systematic, internally coherent method for deriving Islamic law from foundational sources when those sources are ambiguous, incomplete, or silent. Enables thousands of jurists across centuries and territories to reach consistent jurisprudential outcomes through shared methodological principles.
% TRANSFER_FUNCTION: Transfers interpretive authority from textual sources and lay understanding toward the credentialed jurist class trained in rationalist qiyas, ra'y, and istihsan methodology. Moves the power to determine what Islamic law requires — on any unstated question — from democratic deliberation or scriptural literalism toward expert analogical reasoning deployed by authorized scholars.
% ABSENT_VOICES: Hanbali textualists, Maliki practitioners grounding law in Medinan custom, and non-rationalist legal communities relying on folk practice or direct scriptural reading are structurally excluded from controlling the high-level jurisprudential apparatus. They would argue that the Hanafi framework permits unjustified innovation and usurps authority from the primary sources.
% DISAPPEARANCE_RATIONALE: If the Hanafi methodological framework disappeared, Islamic jurisprudence would fragment; the Ottoman and post-Ottoman institutional structures grounding Hanafi legal dominance would lose coherence; regions relying on Hanafi law codes would face immediate reconstitution questions; the professional identity and institutional authority of Hanafi jurists would collapse.
% FOUNDING_PROBLEM: Early Islamic jurisprudence produced inconsistent answers to novel legal questions because scholars applied qiyas, ra'y, and other methods unsystematically and without agreed principles for when each method was permissible. Different territories and jurists reached contradictory conclusions on identical questions.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary Islamic jurisprudence confirms the problem is live: modern questions from biomedics to finance to artificial intelligence have no explicit scriptural answer. Non-Hanafi jurists corroborate the problem's existence; they dispute the Hanafi solution, not its necessity. Shafii and Maliki schools attest to the same coordination problem, proposing alternative methodological solutions.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__hanafi_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__hanafi_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__hanafi_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(usul_al_fiqh_method__hanafi_reading, 'none', 1).
narrative_ontology:epsilon_provenance(usul_al_fiqh_method__hanafi_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__hanafi_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(usul_al_fiqh_method__hanafi_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(usul_al_fiqh_method__hanafi_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness measurement (0.68) reflects the framework's capacity to transfer interpretive authority from textual sources and lay understanding toward the credentialed jurist class. The measurement runs from early institutionalization (0.52) through Ottoman consolidation (0.68), plateauing in the modern period as institutional dominance stabilized. The suppression measurement (0.52) is moderate because the framework is maintained through gatekeeping (requiring years of training in rationalist methodology) rather than external coercion — the mechanism is credentialing, not force. Theater ratio (0.28) indicates that roughly a quarter of enforcement activity is performative maintenance of methodological sophistication — elaborate demonstrations of juristic reasoning that serve to validate the framework's legitimacy as rigorous method — while three-quarters is functional coordination around actual legal problems. The measurements share one grid (every metric at every time point) to enable lifecycle analysis without temporal artifacts. The accessibility_collapse (0.61) reflects that alternatives to the Hanafi method remain intellectually available — a scholar can learn Hanbali or Maliki methodology — but are institutionally costly; the collapse is partial, not complete, because the framework does not physically eliminate alternative reasoning. The resistance (0.71) is substantial because textualist and traditionalist jurists mounted continuous intellectual resistance across centuries, producing sophisticated critiques of qiyas expansion and istihsan deployment.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (textualist jurists, non-rational claimants) experience the Hanafi framework as an extraction mechanism that transfers interpretive authority away from them. The beneficiary seats (Hanafi jurists, rational scholars) experience it as a coordination mechanism that enables coherent law. The agenda-setter seats (Hanafi jurists, Ottoman state) experience it as both — a coordination mechanism they control and an apparatus through which they extract authority and professional rents. The excluded seats (Maliki, Hanbali jurists) experience it as institutional dominance that suppresses their competing methods despite their scholarly credibility.
 *
 * DIRECTIONALITY LOGIC:
 *   The framework's extractiveness flows from its capacity to systematically privilege jurist reasoning over textual fidelity. Beneficiaries gain professional authority and institutional dominance; victims lose the ability to make direct textual claims without navigating the jurist-mediated framework. Identity-lock applies to both beneficiary and victim seats: a Hanafi jurist's professional identity is fused with the rationalist methodology (exit would require abandoning centuries of learning and institutional affiliation); a traditionalist jurist opposing the framework is also identity-locked (their intellectual project is constituted by the opposition, making exit into mere acceptance costly to their self-conception).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — inconsistent application of qiyas and ra'y leading to jurisprudential fragmentation — remains live (confirmed by textualist and Maliki corroborators). The Hanafi solution — systematize analogical reasoning, establish ra'y rules, deploy istihsan for public interest — solves the stated problem. However, the measurement series show extractiveness rising from 0.52 to 0.68 across the Ottoman period and plateauing thereafter, suggesting that the framework's drift is toward extractive authority concentration rather than pure coordination. The theater ratio rising from 0.12 to 0.28 indicates increasing performative maintenance of methodological sophistication — juristic reasoning demonstrated for legitimacy rather than strictly necessary for resolving actual questions. This pattern is consistent with tangled_rope: the framework retains genuine coordination function (deriving law from incomplete sources) alongside asymmetric extraction (concentrating interpretive authority with the credentialed class). The suppression requirement plateaus at 0.52 across the interval, indicating that institutional gatekeeping remains stable — the enforcement does not intensify because the framework achieved institutional entrenchment without needing higher force.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rationalism_vs_textualism_foreclosure,
    'Do the Hanafi commitment to rationalist qiyas/ra''y and the Hanbali commitment to textual minimization logically foreclose each other within a single interpretive framework, or can they coexist as live jurisprudential alternatives?',
    'Historical analysis of whether jurists shifted between schools based on reasoning about methodology (suggesting the positions are genuinely alternative and mutually coherent) or whether the positions were treated as incompatible by their adherents (suggesting genuine foreclosure within traditionalist discourse).',
    'If the positions coexist as live alternatives, the kernel exhibits genuine pluralism and the Hanafi reading''s institutional dominance is contingent and challengeable. If they foreclose each other, the kernel is internally stratified and only one reading can survive at institutional scale.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rationalism_vs_textualism_foreclosure, conceptual, 'Whether Hanafi and Hanbali methodological positions are mutually foreclosing or coexisting alternatives.').

omega_variable(
    istihsan_as_extraction_mechanism,
    'Is istihsan (juristic preference for public interest) genuinely a methodological tool for overriding analogy when justice demands, or does it function primarily as a mechanism through which jurists consolidate authority by claiming to know public interest better than textual sources or analogical logic?',
    'Detailed case analysis of istihsan deployment across centuries: if istihsan consistently favors vulnerable groups and produces outcomes corroborated by non-jurist communities as just, the mechanism is genuine; if istihsan systematically favors jurist institutional interests and contradicts lay understanding, it is primarily extractive.',
    'If istihsan is genuine coordination, the framework''s extractiveness should be lower and tangled_rope classification should weight coordination more heavily. If istihsan is primarily extractive authority-consolidation, the framework should reclassify toward snare (extraction with coordination cover story).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(istihsan_as_extraction_mechanism, empirical, 'Whether istihsan deploys as genuine public-interest protection or as jurist authority-consolidation.').

omega_variable(
    institutional_dominance_contingency,
    'Is the Hanafi framework''s institutional dominance contingent on Ottoman state adoption, or is it grounded in genuine methodological superiority recognized by independent scholars?',
    'Counterfactual analysis: if the Ottoman apparatus had adopted a different school, would Hanafi jurisprudence have retained scholarly credibility and regional influence, or would it have declined below rivals? Evidence: pre-Ottoman Hanafi institutional strength, post-Ottoman Hanafi persistence in non-Hanafi-majority regions.',
    'If dominance is contingent on state backing, the framework is better classified as institutionally maintained extraction (the suppression measurement should be higher). If dominance is independent of state backing, the framework represents genuine scholarly consensus supporting the Hanafi methodology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_dominance_contingency, empirical, 'Whether Hanafi dominance depends on state institutional backing or on independent scholarly credibility.').

omega_variable(
    lay_community_benefit_ambiguity,
    'Do non-jurist communities experience the Hanafi framework as genuinely beneficial (clear, determinate law they can rely on) or as alienating (law derived through expert reasoning they cannot participate in or understand)?',
    'Ethnographic and historical analysis of lay communities'' engagement with Hanafi jurisprudence: do they defer to jurist guidance as legitimate and helpful, or do they resist it as external imposition? Do they supplement Hanafi law with folk practice or alternative interpretations?',
    'If lay communities experience genuine benefit, the framework''s suppression measurement and theater ratio should be lower. If lay communities experience alienation despite clear law, the suppression is higher (internalized exclusion from reasoning) and the framework extracts lay participation from legal decision-making.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(lay_community_benefit_ambiguity, empirical, 'Whether lay communities experience the Hanafi framework as beneficial coordination or as alienating extraction.').

omega_variable(
    reading_identity_lock_mechanism,
    'Why does switching jurisprudential schools (abandoning Hanafi for Hanbali, Maliki, or Shafii methodology) carry such high identity costs? Is this lock grounded in the structural content of the methodology (each school''s internal logic makes alternatives seem incoherent) or in institutional/professional identity fusion (jurists are trained in and committed to one school''s curriculum and institutional structure)?',
    'Analysis of jurists who switched schools: did they describe the move as intellectual discovery (suggesting structural coherence of alternatives) or as institutional displacement and identity disruption (suggesting lock is social rather than logical)?',
    'If lock is structural (methodological coherence), exit options for textualist jurists should be re-rated as more mobile — they could adopt an alternative if persuaded. If lock is institutional, exit remains constrained despite logical possibility. The distinction affects whether resistance comes from genuine conviction or from institutional entrenchment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_identity_lock_mechanism, conceptual, 'Whether jurisprudential identity-lock is structural (methodological) or institutional (professional/social).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__hanafi_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_tr_t0, usul_al_fiqh_method__hanafi_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(usul_tr_t0, observed).
narrative_ontology:measurement(usul_tr_t5, usul_al_fiqh_method__hanafi_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement_basis(usul_tr_t5, observed).
narrative_ontology:measurement(usul_tr_t10, usul_al_fiqh_method__hanafi_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement_basis(usul_tr_t10, observed).
narrative_ontology:measurement(usul_tr_t15, usul_al_fiqh_method__hanafi_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement_basis(usul_tr_t15, observed).
narrative_ontology:measurement(usul_tr_t20, usul_al_fiqh_method__hanafi_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement_basis(usul_tr_t20, observed).
narrative_ontology:measurement(usul_tr_t25, usul_al_fiqh_method__hanafi_reading, theater_ratio, 25, 0.27).
narrative_ontology:measurement_basis(usul_tr_t25, observed).
narrative_ontology:measurement(usul_tr_t30, usul_al_fiqh_method__hanafi_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement_basis(usul_tr_t30, observed).
narrative_ontology:measurement(usul_tr_t40, usul_al_fiqh_method__hanafi_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(usul_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(usul_be_t0, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(usul_be_t0, observed).
narrative_ontology:measurement(usul_be_t5, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 5, 0.56).
narrative_ontology:measurement_basis(usul_be_t5, observed).
narrative_ontology:measurement(usul_be_t10, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement_basis(usul_be_t10, observed).
narrative_ontology:measurement(usul_be_t15, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement_basis(usul_be_t15, observed).
narrative_ontology:measurement(usul_be_t20, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(usul_be_t20, observed).
narrative_ontology:measurement(usul_be_t25, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(usul_be_t25, observed).
narrative_ontology:measurement(usul_be_t30, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(usul_be_t30, observed).
narrative_ontology:measurement(usul_be_t40, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(usul_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(usul_su_t0, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(usul_su_t0, observed).
narrative_ontology:measurement(usul_su_t5, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 5, 0.42).
narrative_ontology:measurement_basis(usul_su_t5, observed).
narrative_ontology:measurement(usul_su_t10, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 10, 0.46).
narrative_ontology:measurement_basis(usul_su_t10, observed).
narrative_ontology:measurement(usul_su_t15, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 15, 0.49).
narrative_ontology:measurement_basis(usul_su_t15, observed).
narrative_ontology:measurement(usul_su_t20, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement_basis(usul_su_t20, observed).
narrative_ontology:measurement(usul_su_t25, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 25, 0.51).
narrative_ontology:measurement_basis(usul_su_t25, observed).
narrative_ontology:measurement(usul_su_t30, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement_basis(usul_su_t30, observed).
narrative_ontology:measurement(usul_su_t40, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 40, 0.52).
narrative_ontology:measurement_basis(usul_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__hanafi_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(usul_al_fiqh_method__hanafi_reading, 0.12).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method__hanbali_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method__maliki_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method__shafii_reading).

% DUAL FORMULATION NOTE:
% The Hanafi reading is one instantiation of the contested kernel usul_al_fiqh_method. All four readings (Hanafi, Hanbali, Maliki, Shafii) share the same referent — the proper method for deriving Islamic law from incomplete textual sources — but differ in which methods are permissible and under what conditions. The Hanafi reading maximizes qiyas expansion, ra'y supplementation, and istihsan deployment; the Hanbali reading minimizes all three in favor of textual fidelity; the Maliki reading incorporates Medinan practice and unrestricted public interest; the Shafii reading systematizes all sources under hadith-authentication hierarchy. Each reading has its own ε (extractiveness toward the credentialed jurist class), distinct victims and beneficiaries, and unique institutional history. The family is linked by network.affects_constraints: Hanafi influences all three siblings by setting the institutional dominance standard they must compete against.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
