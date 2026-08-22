% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__shafii_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usul_al_fiqh_method__shafii_reading, []).

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
 *   constraint_id: usul_al_fiqh_method__shafii_reading
 *   human_readable: Shafi'i Hadith Authentication Hierarchy in Islamic Jurisprudence
 *   domain: legal/religious/intellectual
 *
 * SUMMARY:
 *   The Shafi'i reading of Islamic jurisprudential methodology establishes
 *   authenticated hadith as the gatekeeper for legal derivation, subordinates
 *   analogical reasoning (qiyas) to textual sources, restricts consensus
 *   (ijma) to the Companions' era, and systematizes these hierarchies into
 *   usul al-fiqh as a meta-discipline. This reading instantiates one
 *   interpretation of how Islamic law should derive authority from sources.
 *   It competes with Hanafi emphasis on expansive ra'y, Maliki integration of
 *   Medinan practice and maslaha, and Hanbali maximal textualism. The Shafi'i
 *   reading is chosen for its clarity, historical systematization, and
 *   structural stakes: it produces a tangled rope where genuine coordination
 *   (unified jurisprudence) and asymmetric extraction (gatekeeping authority
 *   transferred to hadith specialists and away from rationalist jurists)
 *   operate through the same constraint.
 *
 * KEY AGENTS:
 *   - hadith_transmission_specialists: Institutional gatekeeper; controls source authentication; beneficiary of methodological hierarchy
 *   - shafii_legal_school: Institutional beneficiary; gains coherent, textually-grounded framework; professional identity fused with methodology
 *   - rationalist_jurists: Powerful but constrained; lose independent authority; must justify reasoning within subordinate framework
 *   - usul_scholars: Meta-discipline interpreters; agenda-setters for the hierarchy itself
 *   - hanafi_legal_school: Excluded from this reading's epistemic framework; represents structural alternative
 *   - muslim_communities: Powerless beneficiaries of unified jurisprudence; constrained payers bearing loss of flexibility
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__shafii_reading, 0.68).
domain_priors:suppression_score(usul_al_fiqh_method__shafii_reading, 0.72).
domain_priors:theater_ratio(usul_al_fiqh_method__shafii_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__shafii_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__shafii_reading, "Shafi'i Hadith Authentication Hierarchy in Islamic Jurisprudence").
narrative_ontology:topic_domain(usul_al_fiqh_method__shafii_reading, "legal/religious/intellectual").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__shafii_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__shafii_reading, 'a36af911-01d6-4cca-a9e4-c83a3a4ef10f').
narrative_ontology:cs_kernel_codification('a36af911-01d6-4cca-a9e4-c83a3a4ef10f', fixed_text).
narrative_ontology:cs_authority_grounding('a36af911-01d6-4cca-a9e4-c83a3a4ef10f', lineage).
narrative_ontology:cs_interpretation_layer_present('a36af911-01d6-4cca-a9e4-c83a3a4ef10f').
narrative_ontology:cs_reading_relation('a36af911-01d6-4cca-a9e4-c83a3a4ef10f', usul_al_fiqh_method__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('a36af911-01d6-4cca-a9e4-c83a3a4ef10f', usul_al_fiqh_method__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('a36af911-01d6-4cca-a9e4-c83a3a4ef10f', usul_al_fiqh_method__hanbali_reading, influences).
narrative_ontology:cs_axiom('a36af911-01d6-4cca-a9e4-c83a3a4ef10f', foundational, hadith_authentication_precedence).
narrative_ontology:cs_axiom_status(hadith_authentication_precedence, holdable).
narrative_ontology:cs_axiom_grounding('a36af911-01d6-4cca-a9e4-c83a3a4ef10f', hadith_authentication_precedence, empirically_contingent).
narrative_ontology:cs_axiom('a36af911-01d6-4cca-a9e4-c83a3a4ef10f', foundational, qiyas_subordination_doctrine).
narrative_ontology:cs_axiom_status(qiyas_subordination_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('a36af911-01d6-4cca-a9e4-c83a3a4ef10f', qiyas_subordination_doctrine, deontological).
narrative_ontology:cs_reference_frame('a36af911-01d6-4cca-a9e4-c83a3a4ef10f', prophetic_transmission_fidelity).
narrative_ontology:cs_drift_state('a36af911-01d6-4cca-a9e4-c83a3a4ef10f', medieval_institutionalization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a36af911-01d6-4cca-a9e4-c83a3a4ef10f', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__shafii_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__shafii_reading, hadith_transmission_specialists).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__shafii_reading, traditionalist_legal_schools).
narrative_ontology:constraint_victim(usul_al_fiqh_method__shafii_reading, rationalist_jurists).
narrative_ontology:constraint_victim(usul_al_fiqh_method__shafii_reading, jurists_claiming_independent_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__shafii_reading, shafii_legal_school).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__shafii_reading, hanbali_legal_school).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__shafii_reading, muslim_communities).
narrative_ontology:constraint_victim(usul_al_fiqh_method__shafii_reading, muslim_communities).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__shafii_reading, textual_fidelity_doctrine).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__shafii_reading, hadith_science_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hadith scholars (muhaddithun) set and enforce the standards by which traditions are authenticated. They develop isnad criticism methods, assess narrator reliability, validate chains of transmission. Their authority rests on the claim that authentication is a specialized science requiring expert judgment. Their professional identity is inseparable from this gatekeeping role. They defend the constraint against attempts to bypass it (by claiming qiyas or ra'y without consulting authenticated hadith). Their institutional position—respected across all schools, consulted on matters of source authenticity—depends on maintaining that legal derivation must begin with authenticated hadith.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, hadith_transmission_specialists, agenda_setter,
    institutional, generational, identity_locked, global).

% The Shafi'i school codifies this methodology as its defining framework. School scholars write treatises systematizing the source hierarchy. Students learn the methodology as the correct way to derive law. The school's reputation and institutional continuity depend on maintaining the methodology's credibility. School scholars benefit from having a clear, teachable framework for legal reasoning. Their professional standing, scholarly authority, and ability to train new generations of Shafi'i jurists all depend on the constraint's persistence. Leaving the constraint would mean dissolving the school's identity.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, shafii_legal_school, beneficiary,
    institutional, generational, identity_locked, global).

% Jurists in the Hanafi tradition and those emphasizing ra'y (reasoned judgment) and istihsan (juristic preference) lose standing under this constraint. They are repeatedly told that their independent reasoning must defer to authenticated hadith, and that their qiyas is only permissible when hadith is absent. Their authority to issue fatwas (legal opinions) is questioned if they cannot cite authenticated hadith support. Their ability to claim independent scholarly standing is limited. They can work within the framework (arguing that their case involves hadith silence and therefore permits qiyas), but they cannot openly claim ra'y as a primary source equal to hadith. Their exit options are constrained: they can migrate to Hanafi or other schools, but leaving the Shafi'i institutional structure means losing its prestige and institutional support. They remain within the Shafi'i framework, subordinated.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, rationalist_jurists, payer,
    powerful, biographical, constrained, regional).

% Scholars of usul al-fiqh (legal methodology) systematize the source hierarchy and become the meta-discipline interpreters. They write the foundational texts on how the sources relate: which applies first, under what conditions, how conflicts are resolved. Their professional expertise is built on the claim that this hierarchy is determinate from Quranic and prophetic principles. Their institutional authority as methodology teachers and textbook authors depends on the constraint's stability and perceived rightness. They defend the hierarchy against both practical challenges (cases where authenticated hadith seems insufficient) and theoretical challenges (arguments that qiyas should expand). Their careers and scholarly reputations are locked into the constraint's persistence.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, usul_scholars, agenda_setter,
    institutional, generational, identity_locked, global).

% The Hanafi school, emphasizing ra'y and qiyas as expansively applicable, is excluded from the Shafi'i reading's epistemic framework. Within Shafi'i institutional space, Hanafi jurisprudence is treated as methodologically unsound—too permissive with reasoning, insufficiently rigorous on authentication. Hanafi scholars can observe and contend with the Shafi'i method, publish their own works, attract their own followers. But they cannot legitimately overturn the Shafi'i hierarchy within the Shafi'i institutional structure. They are voiceless in Shafi'i legal councils and courts. They represent an excluded alternative: what jurisprudence would look like if reasoning were not subordinated to authentication.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, hanafi_legal_school, excluded,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__shafii_reading, hanafi_legal_school, observer).

% The Maliki school integrates Medinan practice ('amal ahl al-Madina) and public interest (maslaha mursala) as independent sources alongside authenticated hadith. Under the Shafi'i constraint, Maliki jurisprudence that proceeds from custom or unrestricted public interest is treated as lacking proper textual grounding. Maliki scholars can maintain their school and issue fatwas within their jurisdictions, but they are excluded from claiming their methodology is superior or equally valid within Shafi'i discourse. They represent an excluded alternative: what jurisprudence would look like if community practice and public benefit were weighted equally with texts.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, maliki_legal_school, excluded,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__shafii_reading, maliki_legal_school, observer).

% The Hanbali school shares the Shafi'i emphasis on textual sources and hadith authenticity. Hanbali scholars often go further: preferring weak hadith over qiyas, blocking innovations through sadd al-dhara'i (blocking means to harm), maximally restricting analogy. Hanbali jurisprudence aligns with and reinforces the Shafi'i constraint's textual emphasis. Hanbali scholars benefit from the broad authentication hierarchy and gain authority from the shared commitment to textual fidelity. They have an allied, though sometimes more restrictive, position. They observe and sometimes challenge the Shafi'i boundaries (arguing it is too permissive with qiyas), but they share the foundational commitment that reasoning must defer to authenticated texts.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, hanbali_legal_school, beneficiary,
    institutional, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__shafii_reading, hanbali_legal_school, observer).

% Muslim communities under Shafi'i jurisprudence gain unified, textually-grounded law. Their fatwas carry authority backed by authenticated sources and systematic reasoning. Their legal disputes are resolved according to clear principles, reducing uncertainty. They also bear costs: their lived practices, customary norms, and community reasoning cannot override the authenticated-hadith hierarchy. If their local practice conflicts with an authenticated hadith, the hadith wins. If they face a novel situation (changing commerce, new family structures), they must fit it into the qiyas framework (analogy to existing hadith), and if no hadith applies closely enough, they may be left without clear guidance. They cannot claim community consensus (ijma) after the Companions' era to override the hierarchy. They are structurally constrained by the methodology even though they also benefit from its coherence.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, muslim_communities, beneficiary,
    powerless, biographical, constrained, universal).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__shafii_reading, muslim_communities, payer).

% Later generations of Muslims (tabi'un and beyond) are excluded from the ijma privilege. The constraint restricts ijma to the Companions' consensus, treating later consensus as less authoritative. Living scholars in later generations who might claim consensus-based authority in their own communities are overruled by this ceiling. They cannot legitimately claim ijma authority; they can only work within qiyas and authentication frameworks. They are voiceless in the derivation hierarchy but subject to rulings derived through it. The constraint denies them the authority their predecessors held.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, companions_descendants, excluded,
    moderate, immediate, trapped, local).

% An external observer sees a constraint that has structured Islamic jurisprudence for over a millennium. The observer notes that it solves a real coordination problem (legal pluralism) through a real mechanism (authenticated-hadith gatekeeping and source hierarchy). The observer also notes the structural asymmetry: hadith specialists gain gatekeeping authority; rationalist jurists lose independent standing; communities lose flexibility; later generations lose ijma authority. The observer sees both the coordination function and the extraction function operating through the same mechanism. The observer measures the constraint's evolution: initial adoption, subsequent institutionalization, increasing theater as the constraint faces challenges.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(usul_al_fiqh_method__shafii_reading, hadith_transmission_specialists).
narrative_ontology:fixing_cost_class(usul_al_fiqh_method__shafii_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Produces unified, textually-grounded Islamic jurisprudence by establishing a clear hierarchy of sources: authenticated hadith first, analogical reasoning second (only when hadith is silent), Companions' consensus as binding, and systematized methodology as the authoritative interpreter. Solves the problem of legal pluralism and inconsistent reasoning by making source authentication the gatekeeper for legitimate derivation.
% TRANSFER_FUNCTION: Transfers interpretive authority from individual reasoning and local practice to hadith specialists and methodologists. Moves decision-making power from the many rationalist jurists (who can claim independent authority) to the fewer hadith authentication specialists (who control source validation). Moves jurisprudential legitimacy from community practice to authenticated textual sources.
% ABSENT_VOICES: Rationalist jurists who claim independent authority through ra'y and istihsan are excluded from the decision-making on whether qiyas should apply; they must answer within a framework not of their choosing. Later generations (post-Companions) are excluded from the ijma privilege. Jurists drawing on Medinan practice or public interest without explicit textual grounding are structurally voiceless. Communities whose customary norms conflict with the authenticated-hadith hierarchy have no standing to override it.
% DISAPPEARANCE_RATIONALE: If the Shafi'i constraint vanished overnight, Islamic jurisprudence would splinter: rationalist methods (Hanafi ra'y, Maliki maslaha, Hanbali weak-hadith preference) would compete equally with hadith authentication; later communities could claim ijma authority; local practice would be re-weighted; qiyas would expand to fill gaps. The entire institutional structure of medieval Islamic legal schools would reorganize around different authority claims. The gatekeeping function that Shafi'i methodology provides would be gone.
% FOUNDING_PROBLEM: Early Islamic jurisprudence was fragmented and inconsistent. Jurists claimed authority through different methods (some emphasizing hadith, others reasoning, others custom). Legal rulings differed across regions for the same issues. There was no agreed framework for determining which sources took priority or how to resolve conflicts between them. Communities and rulers faced uncertainty about the legitimacy of legal decisions.
% FOUNDING_PROBLEM_CORROBORATION: Shafi'i and traditionalist scholars attest the problem remains live: without authenticated-hadith gatekeeping, jurisprudence would again fragment. Rationalist jurists and later Hanafi scholars attest the founding problem is partly solved but at excessive cost: the Shafi'i constraint sacrifices flexibility and community responsiveness for textual purity. Historians and comparative legal scholars, outside the jurisprudential tradition, document that legal pluralism existed and that the Shafi'i methodology was ONE solution among several competing frameworks, not the restoration of a prior unified state.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__shafii_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__shafii_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__shafii_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(usul_al_fiqh_method__shafii_reading, 'none', 1).
narrative_ontology:epsilon_provenance(usul_al_fiqh_method__shafii_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__shafii_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(usul_al_fiqh_method__shafii_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(usul_al_fiqh_method__shafii_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures 0.68 because the constraint transfers substantial interpretive authority (from many rationalist jurists to fewer hadith specialists) and restricts legitimate reasoning paths (qiyas only when hadith is absent; ijma only in Companions' era). Extraction rises from 0.55 to 0.68 across the interval as institutional practices solidify and the methodology becomes more rigorously enforced—early scholars debated the hierarchy; later institutionalization made it doctrine. Suppression measures 0.72 because maintaining the constraint requires active exclusion of alternative methods (Hanafi ra'y, Maliki maslaha) from legitimate discourse and continuous enforcement of the authentication standard against jurists who would expand qiyas or claim independent authority. Theater ratio of 0.41 reflects the constraint's genuine coordination function (unified jurisprudence is real) alongside theatricality: as the constraint matures, increasing shares of effort go to defending the hierarchy against challenge rather than to producing superior jurisprudence (later Hanbalis and rationalists argue the methodology is overextended, yet defenders must emphasize its necessity). Accessibility collapse of 0.64 indicates alternatives remain conceptually available (jurists always could claim ra'y or qiyas; the constraint makes it costly, not impossible) but the constraint's control over institutional legitimacy makes alternatives practically inaccessible within the school's framework. Resistance measures 0.58: the constraint meets real pushback from rationalist schools and from later jurists seeking flexibility, but resistance is muted because (1) schools coexist institutionally rather than eliminating each other, and (2) the Shafi'i framework's genuine coordination benefits make it attractive despite its costs.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (hadith specialists, Shafi'i school, usul scholars) experience this constraint as genuine coordination: a framework for producing superior jurisprudence. The victim seats (rationalist jurists) and excluded seats (Hanafi and Maliki schools) experience it as gatekeeping: authority transferred away from them. The engine should compute these as different types per seat (coordination from the beneficiary side, extraction from the victim side). The foundational structural difference is directionality: hadith specialists and Shafi'i scholars have high stakes in defending the methodology and benefit from its operation; rationalist jurists have high stakes in expanding ra'y and qiyas and are constrained by the hierarchy. Muslim communities sit near symmetric: genuine coordination benefit in unified jurisprudence, but also bearing lost flexibility.
 *
 * DIRECTIONALITY LOGIC:
 *   Hadith transmission specialists: d ≈ 0.15 (beneficiary). They control the gatekeeping function and gain institutional authority; their professional identity is locked into the methodology; exit is not an option. Shafi'i legal school: d ≈ 0.10 (beneficiary). The school's coherence and institutional identity depend on the methodology; abandoning it would dissolve the school. Rationalist jurists: d ≈ 0.80 (victim/target). They lose standing when they cannot cite authenticated hadith; their reasoning is subordinated; they must constantly justify departures. Their exit options are constrained—they can try to expand qiyas within the framework, but leaving the Shafi'i school means losing its institutional backing. Usul scholars: d ≈ 0.05 (beneficiary). They gain professional authority as methodology interpreters; their careers depend on maintaining the hierarchy's necessity. Muslim communities: d ≈ 0.50 (symmetric). They gain unified jurisprudence and clear legal guidance; they lose flexibility and cannot override the hierarchy with local practice. Excluded schools (Hanafi, Maliki): d ≈ 0.75 (structurally excluded targets). They could claim authority through alternative methods, but the Shafi'i constraint overrules them within its institutional space.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids simple mandatrophy misidentification by holding genuine coordination function (unified jurisprudence) alongside asymmetric extraction (gatekeeping transferred to hadith specialists). A reading that claimed this was PURELY coordination (a rope) would fabricate consensus; a reading that claimed it was PURE extraction (a snare) would miss the real problem it solves. The tangled-rope classification captures the structural truth: the coordination problem (legal pluralism, inconsistent reasoning) is real, the solution (authenticated-hadith hierarchy) does coordinate jurisprudence, AND the cost structure is asymmetric (some gain authority, others lose it, through the same mechanism). The theater-ratio drift (0.25 → 0.41) indicates increasing performative maintenance: as the constraint matures and faces rationalist challenges, more institutional effort goes to defending the hierarchy's necessity rather than to producing visibly superior jurisprudence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hadith_authentication_sufficiency,
    'Is the Shafi''i standard for hadith authentication sufficiently stringent and objective to serve as the sole gatekeeper for legal derivation? Or does authentication itself require reasoned judgment (ra''y) that the constraint denies?',
    'Detailed comparison of authentication standards across schools: measure how much disagreement exists on which hadith counts as authenticated. High disagreement suggests authentication itself is contentious; low disagreement suggests the standard is objective enough to be gatekeeping. Also examine whether hadith specialists themselves disagree on authentication, showing that authentication requires the same kind of reasoning the constraint subordinates.',
    'If authentication is shown to require substantial reasoned judgment, the constraint''s claim to gatekeeping becomes circular: it says qiyas must be subordinate to hadith, but hadith authenticity itself requires the kind of reasoning qiyas represents. This would recategorize the constraint as snare (extractive gatekeeping masking itself as textual fidelity) rather than tangled rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hadith_authentication_sufficiency, empirical, 'Whether hadith authentication is objectively verifiable or itself requires reasoned interpretation.').

omega_variable(
    qiyas_necessity_gap,
    'In practice, how often do rationalist jurists encounter cases where authenticated hadith provides no guidance and qiyas must apply? Does the gap between authenticated sources and lived legal situations require expansion of qiyas, as Hanafis argue?',
    'Survey Islamic jurisprudence across centuries: catalog novel legal situations (estate division in new wealth structures, contracts in evolving commerce, family law in changing demographics) and measure how many required qiyas because no hadith applied directly. Compare Shafi''i fatwas (limited qiyas) to Hanafi fatwas (expansive qiyas) on the same issues.',
    'If qiyas is shown to be necessary frequently (high gap), the Shafi''i constraint is functionally incoherent—qiyas must be applied despite the ban, making the constraint more performative than functional. If qiyas is rarely necessary (low gap), the Shafi''i constraint''s restriction is binding and not just theatrical. This affects whether the theater_ratio trend is driven by incoherence or by increasing defensive maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(qiyas_necessity_gap, empirical, 'Whether the Shafi''i restriction on qiyas is functionally adequate for Islamic jurisprudence or creates unsustainable gaps.').

omega_variable(
    methodological_reading_indeterminacy,
    'Is the Shafi''i reading a determinate claim about how Islamic law should work, or is it one internally coherent interpretation of contested textual evidence (Quran and early hadith)? Can the Quran and Sunnah themselves be read as endorsing Hanafi or Maliki methodologies?',
    'Exegetical analysis: compare how Shafi''i scholars justify the source hierarchy from Quranic verses and prophetic practice versus how Hanafi and Maliki scholars justify alternative hierarchies from the same primary sources. If the primary sources admit multiple coherent readings, the Shafi''i reading is not determinate but chosen.',
    'If the reading is internally chosen rather than determinate, the constraint''s legitimacy claim shifts: it is not ''this is what the law requires'' but ''this is our interpretation of the law.'' This reframes the asymmetry from textual fidelity to institutional authority-claim, pushing the constraint toward snare classification (extraction disguised as revealed necessity rather than derived from source priority).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(methodological_reading_indeterminacy, conceptual, 'Whether the Shafi''i source hierarchy is determinate from primary sources or one chosen interpretation among alternatives.').

omega_variable(
    ijma_restriction_scope,
    'Is the restriction of ijma to the Companions'' consensus justified by epistemological grounds (only the Companions had direct access to the Prophet) or does it reflect institutional gatekeeping (later jurists cannot challenge the Companions, preserving methodological stability)?',
    'Examine Shafi''i justifications for the Companions-only ijma rule. If the justification appeals to special epistemic status (proximity to the Prophet), test whether that status is empirically grounded or conceptual. If the justification is institutional (later ijma would be too mutable), the rule is explicitly extractive rather than knowledge-based.',
    'If the rule is knowledge-based, it is harder to classify as pure extraction. If the rule is institutional gatekeeping, it reveals the constraint as suppressing later communities'' ability to claim authority (high d for later Muslims on this axis). This clarifies the direction of extraction and may refine the theater_ratio: the constraint maintains itself partly through epistemic claims (Companions'' special status) and partly through institutional closure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ijma_restriction_scope, conceptual, 'Whether ijma restriction to Companions reflects epistemological principle or institutional gatekeeping.').

omega_variable(
    rationalist_jurist_absorption,
    'Over time, did rationalist jurists accept the Shafi''i constraint, internalize it, and stop claiming alternative authority? Or do they remain structurally excluded, continuously suppressed?',
    'Historical analysis of later Hanafi and Maliki scholars: do they argue for their methodologies as superior, or do they accept Shafi''i superiority and work within it? Measure the frequency of methodological challenge versus methodological acceptance in fatwas and jurisprudential texts over centuries.',
    'If rationalist jurists were absorbed (moved from external suppression to internalized constraint), the suppression metric may be lower than authored. If they remain excluded and challenging, suppression is sustained and the measurement is accurate. This affects interpretation of whether the constraint operates through force (active suppression) or through persuasion (absorption into the framework).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rationalist_jurist_absorption, empirical, 'Whether rationalist jurists accept or resist Shafi''i methodological superiority over centuries.').

omega_variable(
    sibling_reading_foreclosure,
    'Can the Shafi''i reading coexist institutionally with Hanafi and Maliki readings, or does the Shafi''i claim to source authenticity logically foreclose the rationalist alternatives?',
    'Examine whether Shafi''i scholars have ever claimed that Hanafi and Maliki jurisprudence is logically impossible (foreclosed) or merely inferior (coexistent but suboptimal). Compare to whether Hanafis have ever claimed Shafi''i authenticity is incoherent.',
    'If the readings are genuinely mutually foreclosing, the constraint''s persistence requires institutional separation of the schools (they cannot occupy the same legal space). If coexistent, the schools can coexist institutionally while claiming superiority—a structural fact affecting how the constraint operates across different communities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether Shafi''i and rationalist methodologies logically foreclose each other or coexist as alternative frameworks.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__shafii_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_tr_t0, usul_al_fiqh_method__shafii_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(usul_tr_t0, observed).
narrative_ontology:measurement(usul_tr_t2, usul_al_fiqh_method__shafii_reading, theater_ratio, 2, 0.28).
narrative_ontology:measurement_basis(usul_tr_t2, observed).
narrative_ontology:measurement(usul_tr_t4, usul_al_fiqh_method__shafii_reading, theater_ratio, 4, 0.32).
narrative_ontology:measurement_basis(usul_tr_t4, observed).
narrative_ontology:measurement(usul_tr_t6, usul_al_fiqh_method__shafii_reading, theater_ratio, 6, 0.36).
narrative_ontology:measurement_basis(usul_tr_t6, observed).
narrative_ontology:measurement(usul_tr_t8, usul_al_fiqh_method__shafii_reading, theater_ratio, 8, 0.39).
narrative_ontology:measurement_basis(usul_tr_t8, observed).
narrative_ontology:measurement(usul_tr_t10, usul_al_fiqh_method__shafii_reading, theater_ratio, 10, 0.41).
narrative_ontology:measurement_basis(usul_tr_t10, observed).
narrative_ontology:measurement(usul_tr_t12, usul_al_fiqh_method__shafii_reading, theater_ratio, 12, 0.41).
narrative_ontology:measurement_basis(usul_tr_t12, observed).

% Extraction over time
narrative_ontology:measurement(usul_be_t0, usul_al_fiqh_method__shafii_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(usul_be_t0, projected).
narrative_ontology:measurement(usul_be_t2, usul_al_fiqh_method__shafii_reading, base_extractiveness, 2, 0.59).
narrative_ontology:measurement_basis(usul_be_t2, observed).
narrative_ontology:measurement(usul_be_t4, usul_al_fiqh_method__shafii_reading, base_extractiveness, 4, 0.62).
narrative_ontology:measurement_basis(usul_be_t4, observed).
narrative_ontology:measurement(usul_be_t6, usul_al_fiqh_method__shafii_reading, base_extractiveness, 6, 0.65).
narrative_ontology:measurement_basis(usul_be_t6, observed).
narrative_ontology:measurement(usul_be_t8, usul_al_fiqh_method__shafii_reading, base_extractiveness, 8, 0.67).
narrative_ontology:measurement_basis(usul_be_t8, observed).
narrative_ontology:measurement(usul_be_t10, usul_al_fiqh_method__shafii_reading, base_extractiveness, 10, 0.68).
narrative_ontology:measurement_basis(usul_be_t10, observed).
narrative_ontology:measurement(usul_be_t12, usul_al_fiqh_method__shafii_reading, base_extractiveness, 12, 0.68).
narrative_ontology:measurement_basis(usul_be_t12, observed).

% Suppression requirement over time
narrative_ontology:measurement(usul_su_t0, usul_al_fiqh_method__shafii_reading, suppression_requirement, 0, 0.64).
narrative_ontology:measurement_basis(usul_su_t0, observed).
narrative_ontology:measurement(usul_su_t2, usul_al_fiqh_method__shafii_reading, suppression_requirement, 2, 0.67).
narrative_ontology:measurement_basis(usul_su_t2, observed).
narrative_ontology:measurement(usul_su_t4, usul_al_fiqh_method__shafii_reading, suppression_requirement, 4, 0.69).
narrative_ontology:measurement_basis(usul_su_t4, observed).
narrative_ontology:measurement(usul_su_t6, usul_al_fiqh_method__shafii_reading, suppression_requirement, 6, 0.7).
narrative_ontology:measurement_basis(usul_su_t6, observed).
narrative_ontology:measurement(usul_su_t8, usul_al_fiqh_method__shafii_reading, suppression_requirement, 8, 0.71).
narrative_ontology:measurement_basis(usul_su_t8, observed).
narrative_ontology:measurement(usul_su_t10, usul_al_fiqh_method__shafii_reading, suppression_requirement, 10, 0.72).
narrative_ontology:measurement_basis(usul_su_t10, observed).
narrative_ontology:measurement(usul_su_t12, usul_al_fiqh_method__shafii_reading, suppression_requirement, 12, 0.72).
narrative_ontology:measurement_basis(usul_su_t12, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__shafii_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(usul_al_fiqh_method__shafii_reading, 0.12).
narrative_ontology:affects_constraint(usul_al_fiqh_method__shafii_reading, usul_al_fiqh_method__hanafi_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__shafii_reading, usul_al_fiqh_method__maliki_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__shafii_reading, usul_al_fiqh_method__hanbali_reading).

% DUAL FORMULATION NOTE:
% The Shafi'i reading is one of four decomposed constraints from the kernel usul_al_fiqh_method. Each constraint instantiates a different reading with different source hierarchies, beneficiaries, and extraction profiles. The Shafi'i reading emphasizes hadith authentication gatekeeping; Hanafi emphasizes expansive rationalist reasoning; Maliki integrates Medinan practice and public interest; Hanbali maximizes textual restriction. Each reading is a separate constraint story with its own beneficiary/victim structure and ε value. The network links them as a constraint family: the Shafi'i reading influences all others by establishing authentication standards that later readings either subordinate (Hanafi) or supplement (Maliki) or exceed (Hanbali).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(usul_al_fiqh_method__shafii_reading, moderate, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
