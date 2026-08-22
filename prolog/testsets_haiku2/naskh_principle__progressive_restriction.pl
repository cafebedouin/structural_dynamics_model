% ============================================================================
% CONSTRAINT STORY: naskh_principle__progressive_restriction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_naskh_principle__progressive_restriction, []).

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
 *   constraint_id: naskh_principle__progressive_restriction
 *   human_readable: Progressive Restriction Reading of Quranic Revelation (Naskh Principle)
 *   domain: religious/legal/hermeneutical
 *
 * SUMMARY:
 *   The progressive-restriction reading of naskh (abrogation) is a
 *   hermeneutical framework that resolves apparent Quranic contradictions by
 *   treating the movement from permissive to restrictive verses as divine
 *   pedagogy rather than textual supersession. Early-revealed verses
 *   permitting practices (alcohol, certain marriage forms, slavery,
 *   aggressive warfare) are understood as transitional accommodations suited
 *   to the community's spiritual capacity at that stage; later-revealed
 *   restrictions represent the final divine intent. This reading has become
 *   institutionalized in many contemporary Islamic legal systems and
 *   scholarly establishments, where it serves to justify restrictive law
 *   while honoring the full canon of revelation. The constraint operates as a
 *   hermeneutical gate: it transfers authority to interpret binding law from
 *   those who cite early permissive verses to progressive scholars and
 *   institutions who declare the restriction trajectory final. The extracted
 *   cost is borne by literalist readers and practitioners who cite early
 *   verses for contemporary practice—their textual warrant is downgraded to
 *   historical accommodation. This is a kernel reading, one of three
 *   competing interpretations of the same textual and theological problem.
 *
 * KEY AGENTS:
 *   - progressive_legal_scholars: institutional agenda-setters who author and promote the progressive-restriction framework via exegesis, fatwa, and education
 *   - reformist_exegetical_tradition: organized network of universities, councils, and publishers that benefits from the reading's institutional adoption
 *   - literalist_permissive_readings: scholarly positions bearing the cost of having their textual citations downgraded to transitional accommodation
 *   - contemporary_practitioners_citing_early_verses: powerless, identity-locked agents excluded from hermeneutical authority but constrained by the reading's institutional dominance
 *   - classical_abrogation_scholars: excluded institutional competitors whose chronological framework the progressive reading implicitly supplants
 *   - contextual_harmonization_scholars: excluded institutional competitors whose all-verses-valid approach the progressive reading marginalizes
 *   - islamic_legal_institutions: dual agenda-setter and beneficiary, enforcing the reading in courts and fatwa councils
 *   - contemporary_society_enforcing_restrictions: powerful beneficiary—modern jurisdictions relying on progressive-restriction warrant for restrictive law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(naskh_principle__progressive_restriction, 0.62).
domain_priors:suppression_score(naskh_principle__progressive_restriction, 0.58).
domain_priors:theater_ratio(naskh_principle__progressive_restriction, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, extractiveness, 0.62).
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, accessibility_collapse, 0.67).
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(naskh_principle__progressive_restriction, tangled_rope).
narrative_ontology:human_readable(naskh_principle__progressive_restriction, "Progressive Restriction Reading of Quranic Revelation (Naskh Principle)").
narrative_ontology:topic_domain(naskh_principle__progressive_restriction, "religious/legal/hermeneutical").

domain_priors:requires_active_enforcement(naskh_principle__progressive_restriction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(naskh_principle__progressive_restriction, '7f28de7b-2236-4644-986e-32cfc71c1c1d').
narrative_ontology:cs_kernel_codification('7f28de7b-2236-4644-986e-32cfc71c1c1d', fixed_text).
narrative_ontology:cs_authority_grounding('7f28de7b-2236-4644-986e-32cfc71c1c1d', lineage).
narrative_ontology:cs_interpretation_layer_present('7f28de7b-2236-4644-986e-32cfc71c1c1d').
narrative_ontology:cs_reading_relation('7f28de7b-2236-4644-986e-32cfc71c1c1d', naskh_principle__classical_abrogation, influences).
narrative_ontology:cs_reading_relation('7f28de7b-2236-4644-986e-32cfc71c1c1d', naskh_principle__contextual_harmonization, coexists_with).
narrative_ontology:cs_axiom('7f28de7b-2236-4644-986e-32cfc71c1c1d', foundational, revelation_as_pedagogical_trajectory).
narrative_ontology:cs_axiom_status(revelation_as_pedagogical_trajectory, holdable).
narrative_ontology:cs_axiom_grounding('7f28de7b-2236-4644-986e-32cfc71c1c1d', revelation_as_pedagogical_trajectory, deontological).
narrative_ontology:cs_axiom('7f28de7b-2236-4644-986e-32cfc71c1c1d', foundational, restriction_represents_final_divine_intent).
narrative_ontology:cs_axiom_status(restriction_represents_final_divine_intent, holdable).
narrative_ontology:cs_axiom_grounding('7f28de7b-2236-4644-986e-32cfc71c1c1d', restriction_represents_final_divine_intent, empirically_contingent).
narrative_ontology:cs_reference_frame('7f28de7b-2236-4644-986e-32cfc71c1c1d', revealed_text_as_unified_pedagogical_whole).
narrative_ontology:cs_drift_state('7f28de7b-2236-4644-986e-32cfc71c1c1d', contemporary_institutional_enforcement, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7f28de7b-2236-4644-986e-32cfc71c1c1d', '').
narrative_ontology:cs_kernel_id(naskh_principle__progressive_restriction, naskh_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(naskh_principle__progressive_restriction, progressive_legal_scholars).
narrative_ontology:constraint_beneficiary(naskh_principle__progressive_restriction, reformist_exegetical_tradition).
narrative_ontology:constraint_victim(naskh_principle__progressive_restriction, literalist_permissive_readings).
narrative_ontology:constraint_victim(naskh_principle__progressive_restriction, contemporary_practitioners_citing_early_verses).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(naskh_principle__progressive_restriction, islamic_legal_institutions).
narrative_ontology:constraint_beneficiary(naskh_principle__progressive_restriction, contemporary_society_enforcing_restrictions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Academic and juridical authorities who adopt the progressive-restriction reading to justify contemporary restrictions on practices permitted in earlier-revealed verses (e.g., alcohol initially permitted, later prohibited; marriage practices expanded then constrained). They set the hermeneutical agenda by producing tafsir (exegesis), fatwa (legal opinion), and doctoral dissertations that frame the restriction trajectory as divine pedagogy. This reading serves their intellectual project of reconciling historical practices with modern ethics and institutional evolution.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, progressive_legal_scholars, agenda_setter,
    institutional, generational, arbitrage, global).

% A scholarly and institutional network spanning universities, fatwa councils, and publishing houses that benefit from the progressive-restriction framework by gaining hermeneutical authority to declare contemporary restrictions final and binding. The reading vindicates their interpretive method and positions them as guardians of evolved divine intent. Their legitimacy grows as the restriction reading becomes canonical in educational and juridical institutions.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, reformist_exegetical_tradition, beneficiary,
    organized, generational, mobile, global).

% Scholarly positions that cite early-revealed permissive verses (such as initial Quranic statements on alcohol, interest, marriage terms, or warfare conduct) to argue those practices remain valid until explicitly textually abrogated. They bear the cost of the progressive-restriction framework because it downgrades their textual citations from evidence to 'transitional accommodations,' removing them from contemporary jurisprudential force. Their exit from this constraint is constrained by the canonical status the progressive reading has acquired in institutional Islam.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, literalist_permissive_readings, payer,
    moderate, biographical, constrained, global).

% Communities and individuals who appeal to early-revealed permissive verses to justify contemporary practices (e.g., citing initial permission for wine-drinking in early Islamic history, marriage customs from Quranic examples, or warfare precedents). The progressive-restriction reading suppresses their textual warrant by declaring those verses pedagogical stepping-stones, not permanent law. They are excluded from the hermeneutical conversation because they lack scholarly credentials to mount counter-exegesis; their identity as believers committed to the Quran and Sunnah makes exit to an alternative legal system unthinkable.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, contemporary_practitioners_citing_early_verses, payer,
    powerless, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(naskh_principle__progressive_restriction, contemporary_practitioners_citing_early_verses, excluded).

% Exegetical authorities and legal schools (particularly classical madhab jurisprudence) that employ the abrogation (naskh) framework to resolve contradictions: later verses abrogate earlier ones via formal chronological supersession. The progressive-restriction reading positions itself as a more nuanced alternative that preserves the dignity and pedagogical wisdom of earlier verses rather than declaring them abrogated (invalidated). This reading implicitly challenges the classical framework's authority to adjudicate revelation order and supersession, even while claiming to honor earlier revealed material.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, classical_abrogation_scholars, excluded,
    institutional, generational, constrained, global).

% Exegetical authorities who argue all Quranic verses remain valid within their specific historical, cultural, or spiritual contexts without invoking chronological abrogation or progressive restriction. They are excluded from this reading's framework because progressive restriction relies on a historical-evolution narrative (restriction as pedagogical trajectory) that the harmonization reading rejects in favor of simultaneous multi-contextual validity.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, contextual_harmonization_scholars, excluded,
    institutional, generational, constrained, global).

% Courts, religious authorities, educational institutions, and state-backed fatwa councils that adopt or enforce the progressive-restriction reading in their jurisprudential output. They benefit from the clarity the reading provides (earlier permissive verses are understood as transitional; later restrictions are final) and from the intellectual legitimacy it lends to institutional policy that enforces the later restrictions. The reading is often embedded in statutory interpretation and institutional precedent, making it the default hermeneutical framework.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, islamic_legal_institutions, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(naskh_principle__progressive_restriction, islamic_legal_institutions, beneficiary).

% Modern Muslim-majority societies, secular nation-states with Muslim populations, and international religious authorities that rely on the progressive-restriction reading to justify and enforce contemporary moral and legal restrictions (on alcohol, interest-bearing finance, certain marriage and divorce practices, warfare conduct). The reading provides textual warrant for restrictive law in a modern context while neutralizing appeals to early verses that permitted or accepted those practices. They benefit by gaining hermeneutical cover for restrictions that serve modern institutional and social order.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, contemporary_society_enforcing_restrictions, beneficiary,
    powerful, generational, arbitrage, global).

% Historians of Islamic jurisprudence, comparative legal scholars, and academic observers who study how the progressive-restriction reading operates as a hermeneutical gate, who benefits, and what alternatives remain open. They are not parties to the constraint but study its structure from outside the believer's internal hermeneutical struggle.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, quranic_scholarship_analytical_seat, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(naskh_principle__progressive_restriction, progressive_legal_scholars).
narrative_ontology:fixing_cost_class(naskh_principle__progressive_restriction, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reconciles the apparent textual contradiction between early-revealed permissive verses and later-revealed restrictive verses by framing the progression as intentional divine pedagogy: communities progress from permission toward restriction as their spiritual and moral capacity develops. This resolves exegetical tension without requiring formal abrogation (naskh in the classical sense), which some scholars view as problematic because it implies earlier revelation was defective.
% TRANSFER_FUNCTION: Moves hermeneutical authority from literalist readers of early-permissive verses to progressive scholars and institutions who interpret the restriction trajectory as final divine intent. Early verses lose jurisprudential force in contemporary application; their citations are recast as historical accommodation. This transfers the capacity to derive binding law from the textual surface (permissive verses) to the interpretive institutional structure (the scholarly consensus that restrictions represent evolved divine intention).
% ABSENT_VOICES: Literalist scholars and non-credentialed believers who cite early verses to justify contemporary practices are excluded because they lack institutional authority to mount counter-exegesis. Communities in historically restrictive jurisdictions who would appeal to early permissive verses have no seat at the hermeneutical table. Classical abrogation scholars are excluded because the progressive reading implicitly supersedes their classical framework, yet they retain institutional authority in some contexts, creating contestation.
% DISAPPEARANCE_RATIONALE: If the progressive-restriction reading disappeared and were replaced by literal equality of all verses (none more binding than others) or by pure contextual harmonization, contemporary Islamic jurisprudence would rearrange: statutory law in many Muslim-majority countries rests on the authority of restrictive verses understood as final; without that framework, the permissive earlier verses would re-enter jurisprudential play. Divorce law, financial law, and law on consumption of stimulants would all face renewed contestation. The institutional legitimacy of many fatwa councils and legal scholars depends on this reading being authoritative.
% FOUNDING_PROBLEM: Early Islamic revelation contained verses permitting practices (alcohol consumption, forms of marriage, slavery, warfare conduct) that later verses restricted or prohibited. Classical Islamic jurisprudence developed the doctrine of abrogation (naskh) to resolve the contradiction chronologically: later verses supersede earlier ones. However, some scholars found this framework theologically unsatisfying because it implied early revelation was provisional or defective. The progressive-restriction reading was developed to preserve the dignity of early verses while still allowing later restrictions to be binding law: they represent divine pedagogy, not correction of error.
% FOUNDING_PROBLEM_CORROBORATION: Progressive scholars and institutional exegetes attest the founding problem is live: reconciling textual contradiction while honoring all revelation is essential to Islamic faith. Classical abrogation scholars attest the founding problem was solved by chronological supersession doctrine centuries ago; progressive restriction is an unnecessary complication. Contextual harmonization scholars attest the problem disappears when you recognize all verses as valid-in-context. Literary scholars and historians of jurisprudence outside the Islamic tradition corroborate that this is a genuine tension in the textual material; opinions diverge on which resolution is most coherent.
narrative_ontology:disappearance_verdict(naskh_principle__progressive_restriction, world_rearranges).
narrative_ontology:founding_problem_status(naskh_principle__progressive_restriction, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(naskh_principle__progressive_restriction, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(naskh_principle__progressive_restriction, 'none', 1).
narrative_ontology:epsilon_provenance(naskh_principle__progressive_restriction, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(naskh_principle__progressive_restriction_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(naskh_principle__progressive_restriction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(naskh_principle__progressive_restriction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is claimed as tangled_rope because it possesses both genuine coordination (reconciling textual contradiction via a coherent pedagogical narrative) and asymmetric extraction (transferring hermeneutical authority from literal-text readers to institutional progressives). Extractiveness is moderate-high (0.62 at interval end) because the reading systematically removes jurisprudential force from early verses, benefiting modern restrictive law while constraining appeals to historical permissiveness. Suppression is moderate (0.58) because the reading does not formally ban alternative interpretations but subordinates them institutionally—literalist scholars can still publish, but their conclusions are treated as historically outdated. Theater ratio is moderate (0.41): the exegetical justification is intellectually sophisticated and invokes genuine textual patterns (progressive restriction does occur in revelation), but an increasing share of institutional effort goes to suppressing permissive-verse citations rather than developing the pedagogical theory itself. Accessibility collapse is moderate-high (0.67) because once the progressive-restriction narrative is accepted, appealing to early permissive verses appears naive or literalist—alternatives collapse epistemically. Resistance is high (0.72) because literalist and classical-abrogation scholars mount sustained counter-arguments, and communities citing early verses resist the institutional closure. Measurement series show extractiveness and theater rising over the interval (from 0.38/0.22 at t=0 to 0.62/0.41 at t=40): as the reading became institutionalized in modern Islamic jurisprudence and education, both its claimed pedagogical function and its actual suppression of permissive citations have increased.
 *
 * PERSPECTIVAL GAP:
 *   The progressive scholar (agenda-setter) and the contemporary practitioner (payer) should compute different types. From the progressive seat, the reading is genuine coordination—it solves a real textual problem via an intellectually satisfying pedagogical narrative that honors all revelation. From the practitioner's seat, the same structure operates as enforced extraction: their textual warrant (early verses) is suppressed by institutional reinterpretation; they cannot exit without abandoning their identity as Quranic believers. The engine computes this seat-divergence from the structural data: the scholar has arbitrage exit (can move between interpretive schools, has institutional mobility), while the practitioner has identity-locked exit (cannot abandon Islam or the Quran without violating their foundational self-concept). Directive flow differs correspondingly: the scholar directs restriction, the practitioner bears its suppression.
 *
 * DIRECTIONALITY LOGIC:
 *   Progressive scholars and the reformist tradition derive clear benefit (d near 0.0, beneficiary end): they gain hermeneutical authority, institutional prestige, and the capacity to declare their interpretation binding law. Islamic legal institutions and contemporary restrictive societies benefit similarly. Literalist and permissive readers bear the cost: their textual citations are downgraded; they face institutional suppression without formal censorship. The identity-locked powerless practitioner is maximally constrained: they cannot appeal to early verses (suppressed), cannot adopt the progressive reading (it contradicts their literalist commitments), and cannot exit Islam itself (identity-fused). Their directionality sits at the high-extraction end (d near 1.0). Classical and contextual scholars are excluded but institutional: they retain some capacity to contest the reading's canonical authority, so their directionality is moderate-high (d ~0.65)—they pay opportunity costs (their frameworks are marginalized) but are not fully trapped. Overrides are not required; the structural derivation from beneficiary/victim + exit should capture these relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The progressive-restriction reading avoids simple mandatrophy because it possesses a genuine coordination function: it does solve a real textual-hermeneutical problem (reconciling contradictory verses) via a conceptually sophisticated framework (divine pedagogy). The founding problem (early verses permit, later verses restrict—how do we honor both?) remains live in Islamic jurisprudence; the progressive reading offers a coherent answer. However, mandatrophy risk exists because the primary institutional function has begun to shift: the reading was originally developed to preserve textual dignity while allowing institutional evolution, but modern institutional Islam increasingly uses it to enforce restrictive law against contemporary appeals to early permissive verses. The exegetical justification is intellectually real, but its institutional deployment increasingly suppresses alternative readings without engaging their substance. The theater ratio (0.41, rising) captures this: a growing share of enforcement effort defends the restriction-finale claim against literalist counter-argument rather than developing the pedagogical theory itself. If the theater ratio crosses 0.5, the constraint enters piton territory (atrophied coordination, performance-maintained extraction).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pedagogical_intent_vs_institutional_suppression,
    'Is the progressive-restriction reading fundamentally about pedagogical reconciliation of revealed texts, or has it become primarily a mechanism for institutional suppression of permissive-verse citations in modern jurisprudence?',
    'Qualitative analysis of contemporary fatwa councils, legal scholar citations, and educational curricula: examine whether modern institutional deployment emphasizes the pedagogical theory or focuses on excluding permissive-verse arguments. Track the ratio of exegetical development to jurisprudential enforcement over time.',
    'If the reading has become primarily suppressive despite its pedagogical framing, it crosses from tangled_rope (genuine coordination + asymmetric extraction) toward snare (extraction dressed in coordination narrative). This would suggest mandatrophy risk: the founding coordination problem is solved, but the institutional structure persists to extract via hermeneutical gatekeeping.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pedagogical_intent_vs_institutional_suppression, empirical, 'Whether the reading''s institutional function remains aligned with its theoretical justification.').

omega_variable(
    alternative_reading_coexistence,
    'Can the progressive-restriction reading coexist in the same juridical framework with classical-abrogation or contextual-harmonization readings, or does one reading''s institutional dominance foreclose the others?',
    'Examine plural-school Islamic jurisdictions (e.g., Ottoman millet system, contemporary multi-madhab councils) to see whether different readings are formally recognized as equally valid or whether one is institutionally imposed as canonical. Study fatwa-council procedures to determine if alternative readings receive substantive hearing or ritual deference only.',
    'If the readings can coexist in institutional pluralism, they operate as a genuine contestation (coexists_with relation holds). If institutional adoption of the progressive reading forecloses others (schools teaching only this reading, fatwa councils rejecting alternative arguments without engagement), the relation is stronger—possibly forecloses or influences. This affects classification at the kernel level.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_reading_coexistence, empirical, 'Institutional capacity for plural readings of the same kernel.').

omega_variable(
    identity_lock_mechanism_for_constrained_agents,
    'Is the identity-lock experienced by practitioners who cite early verses structurally inherent to Islamic belief (cannot abandon the Quran), or is it reinforced by the institutional dominance of the progressive-restriction reading?',
    'Compare Muslim communities and jurists who operate outside institutional progressivism: examine whether they retain psychological/social capacity to cite early verses freely, or whether even in non-institutionalized contexts they self-censor due to internalized progressive-reading authority. Test via ethnographic research and qualitative interviews.',
    'If identity-lock is purely structural (belief in Quran''s integrity is irreversible), the constraint''s suppression is lower than the institutional suppression score suggests—exit is truly impossible. If identity-lock is partially internalized through institutional authority, the constraint carries higher effective suppression than the structural measures indicate. This affects computation of effective extraction (χ) for the powerless agent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_for_constrained_agents, empirical, 'Degree to which suppression of permissive-verse citations is structural versus institutionally reinforced.').

omega_variable(
    reading_contest_as_kernel_vs_settled_doctrine,
    'Is the naskh principle best understood as a contested kernel (three live, incompatible readings) or as a settled doctrine (progressive restriction is canonical Islamic jurisprudence, alternatives are fringe or historical)?',
    'Survey fatwa-council membership and jurisprudential education curricula across Muslim-majority regions: measure institutional representation of each reading. Determine whether classical abrogation and contextual harmonization are taught as live jurisprudential options or as historical artifacts.',
    'If the progressive reading is truly canonical and alternatives are residual, the kernel framing may overstate the contestation—this would be a settled tangled_rope with extinct alternatives, not an active kernel. If all three readings retain institutional footholds (different schools, regions, contemporary scholars), the kernel framing is accurate and three separate constraint stories should be authored to capture the full semantic/institutional reality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_as_kernel_vs_settled_doctrine, empirical, 'Whether naskh_principle is a live contested kernel or a resolved doctrine with residual alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(naskh_principle__progressive_restriction, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nask_tr_t0, naskh_principle__progressive_restriction, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(nask_tr_t0, observed).
narrative_ontology:measurement(nask_tr_t5, naskh_principle__progressive_restriction, theater_ratio, 5, 0.26).
narrative_ontology:measurement_basis(nask_tr_t5, observed).
narrative_ontology:measurement(nask_tr_t10, naskh_principle__progressive_restriction, theater_ratio, 10, 0.31).
narrative_ontology:measurement_basis(nask_tr_t10, observed).
narrative_ontology:measurement(nask_tr_t15, naskh_principle__progressive_restriction, theater_ratio, 15, 0.36).
narrative_ontology:measurement_basis(nask_tr_t15, observed).
narrative_ontology:measurement(nask_tr_t25, naskh_principle__progressive_restriction, theater_ratio, 25, 0.39).
narrative_ontology:measurement_basis(nask_tr_t25, observed).
narrative_ontology:measurement(nask_tr_t40, naskh_principle__progressive_restriction, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(nask_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(nask_be_t0, naskh_principle__progressive_restriction, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(nask_be_t0, observed).
narrative_ontology:measurement(nask_be_t5, naskh_principle__progressive_restriction, base_extractiveness, 5, 0.42).
narrative_ontology:measurement_basis(nask_be_t5, observed).
narrative_ontology:measurement(nask_be_t10, naskh_principle__progressive_restriction, base_extractiveness, 10, 0.48).
narrative_ontology:measurement_basis(nask_be_t10, observed).
narrative_ontology:measurement(nask_be_t15, naskh_principle__progressive_restriction, base_extractiveness, 15, 0.54).
narrative_ontology:measurement_basis(nask_be_t15, observed).
narrative_ontology:measurement(nask_be_t25, naskh_principle__progressive_restriction, base_extractiveness, 25, 0.6).
narrative_ontology:measurement_basis(nask_be_t25, observed).
narrative_ontology:measurement(nask_be_t40, naskh_principle__progressive_restriction, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(nask_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(nask_su_t0, naskh_principle__progressive_restriction, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(nask_su_t0, observed).
narrative_ontology:measurement(nask_su_t5, naskh_principle__progressive_restriction, suppression_requirement, 5, 0.45).
narrative_ontology:measurement_basis(nask_su_t5, observed).
narrative_ontology:measurement(nask_su_t10, naskh_principle__progressive_restriction, suppression_requirement, 10, 0.5).
narrative_ontology:measurement_basis(nask_su_t10, observed).
narrative_ontology:measurement(nask_su_t15, naskh_principle__progressive_restriction, suppression_requirement, 15, 0.54).
narrative_ontology:measurement_basis(nask_su_t15, observed).
narrative_ontology:measurement(nask_su_t25, naskh_principle__progressive_restriction, suppression_requirement, 25, 0.57).
narrative_ontology:measurement_basis(nask_su_t25, observed).
narrative_ontology:measurement(nask_su_t40, naskh_principle__progressive_restriction, suppression_requirement, 40, 0.58).
narrative_ontology:measurement_basis(nask_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(naskh_principle__progressive_restriction, identity_coordination).
narrative_ontology:boltzmann_floor_override(naskh_principle__progressive_restriction, 0.12).
narrative_ontology:affects_constraint(naskh_principle__progressive_restriction, naskh_principle__classical_abrogation).
narrative_ontology:affects_constraint(naskh_principle__progressive_restriction, naskh_principle__contextual_harmonization).

% DUAL FORMULATION NOTE:
% The naskh_principle kernel decomposes into three structurally distinct constraints, each instantiating a different reading: classical_abrogation (chronological supersession, beneficiaries are juridical clarity seekers), contextual_harmonization (all-verses-valid-in-context, beneficiaries are literalists), progressive_restriction (this story—pedagogical trajectory, beneficiaries are progressives). The three readings share a common kernel (Quranic verses appear contradictory) but construct different cs_pattern architectures, beneficiary/victim structures, and extraction profiles. Each reading should be authored as a separate constraint story with its own epsilon, its own stakeholders, and its own six-questions interview. This story covers only progressive_restriction; its siblings are separate constraint files linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
