% ============================================================================
% CONSTRAINT STORY: classical_latin_standard__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_classical_latin_standard__hybrid_reading, []).

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
 *   constraint_id: classical_latin_standard__hybrid_reading
 *   human_readable: Hybrid Classical Standard for Learned Latin (Textual Fidelity with Domain Accommodation)
 *   domain: historical linguistics/philology/commitment systems
 *
 * SUMMARY:
 *   When the philological recovery of the fifteenth century put the Classical
 *   corpus back into circulation, learned Europe discovered that its working
 *   Latin — the vehicle of law, theology, medicine, and administration for a
 *   millennium — no longer matched the recovered exemplars. Three settlements
 *   competed. The hybrid reading, consolidated through humanist schooling,
 *   printing-house normalization, and curial practice, holds that correct
 *   Latin binds writers to Classical grammatical norms while granting
 *   recognized technical and ecclesiastical registers license to keep their
 *   post-Classical vocabulary. It solves a real coordination problem — a
 *   Krakow dissertation readable in Salamanca, one liturgical language from
 *   Lima to Lisbon — and simultaneously extracts status from writers formed
 *   outside the Classical curriculum, whose inherited idiom is reclassified
 *   as barbarism requiring correction. The victim set is deliberately
 *   narrower than the reconstruction reading's (only condemned forms, not all
 *   drift) and the beneficiary set broader than the continuity reading's
 *   would be. Epsilon's referent is the standing hybrid arrangement itself,
 *   assessed by this reading's own lights: the accommodation it grants is
 *   counted as legitimate, the delegitimization it imposes is counted as
 *   cost. The claim/metric independence rule applies: claimed_type is
 *   authored from the structural facts (real coordination plus asymmetric
 *   extraction plus active enforcement), and the metrics are authored from
 *   the descriptive record, without tuning either to the other.
 *
 * KEY AGENTS:
 *   - - classical_philologists: agenda-setter and principal beneficiary (institutional/identity_locked) — administers the standard through schools, editorships, and curial style offices; collects the deference paid to Classical mastery
 *   - - ecclesiastical_curia: institutional beneficiary (institutional/constrained) — keeps its technical vocabulary under the standard's protection; largest continuous Latin-writing enterprise
 *   - - university_faculties: beneficiary (institutional/constrained) — teach and certify the standard; gain a portable international curriculum, bear the enforcement labor
 *   - - scholastic_theologians: primary payer (organized/identity_locked) — their method's technical idiom is reclassified as barbarism; method and language are one training
 *   - - provincial_clergy: payer (moderate/constrained) — vernacular-influenced Latin corrected from above; advancement priced in compliance
 *   - - printing_houses: beneficiary and enforcement amplifier (powerful/mobile) — editorial normalization widens their institutional market
 *   - - living_latin_advocates: excluded (organized/trapped) — would defend contemporary usage as the criterion; hold no seat in the arbitration machinery
 *   - - philological_historians: analytical observer (analytical/analytical) — reconstructs the standard's formation and retreat from outside the game
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(classical_latin_standard__hybrid_reading, 0.46).
domain_priors:suppression_score(classical_latin_standard__hybrid_reading, 0.45).
domain_priors:theater_ratio(classical_latin_standard__hybrid_reading, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, extractiveness, 0.46).
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(classical_latin_standard__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(classical_latin_standard__hybrid_reading, "Hybrid Classical Standard for Learned Latin (Textual Fidelity with Domain Accommodation)").
narrative_ontology:topic_domain(classical_latin_standard__hybrid_reading, "historical linguistics/philology/commitment systems").

domain_priors:requires_active_enforcement(classical_latin_standard__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(classical_latin_standard__hybrid_reading, '787c320a-fc5d-4c51-bddc-2be298acc537').
narrative_ontology:cs_kernel_codification('787c320a-fc5d-4c51-bddc-2be298acc537', fixed_text).
narrative_ontology:cs_authority_grounding('787c320a-fc5d-4c51-bddc-2be298acc537', lineage).
narrative_ontology:cs_interpretation_layer_present('787c320a-fc5d-4c51-bddc-2be298acc537').
narrative_ontology:cs_reading_relation('787c320a-fc5d-4c51-bddc-2be298acc537', classical_latin_standard__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('787c320a-fc5d-4c51-bddc-2be298acc537', classical_latin_standard__reconstruction_reading, coexists_with).
narrative_ontology:cs_axiom('787c320a-fc5d-4c51-bddc-2be298acc537', foundational, classical_texts_bind_grammatical_correctness).
narrative_ontology:cs_axiom_status(classical_texts_bind_grammatical_correctness, holdable).
narrative_ontology:cs_axiom_grounding('787c320a-fc5d-4c51-bddc-2be298acc537', classical_texts_bind_grammatical_correctness, conventional).
narrative_ontology:cs_axiom('787c320a-fc5d-4c51-bddc-2be298acc537', foundational, post_classical_technical_registers_are_legitimate).
narrative_ontology:cs_axiom_status(post_classical_technical_registers_are_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('787c320a-fc5d-4c51-bddc-2be298acc537', post_classical_technical_registers_are_legitimate, instrumental).
narrative_ontology:cs_reference_frame('787c320a-fc5d-4c51-bddc-2be298acc537', textual_fidelity_with_domain_accommodation).
narrative_ontology:cs_drift_state('787c320a-fc5d-4c51-bddc-2be298acc537', contemporary_learned_practice, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('787c320a-fc5d-4c51-bddc-2be298acc537', '').
narrative_ontology:cs_kernel_id(classical_latin_standard__hybrid_reading, classical_latin_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(classical_latin_standard__hybrid_reading, classical_philologists).
narrative_ontology:constraint_beneficiary(classical_latin_standard__hybrid_reading, ecclesiastical_curia).
narrative_ontology:constraint_beneficiary(classical_latin_standard__hybrid_reading, university_faculties).
narrative_ontology:constraint_beneficiary(classical_latin_standard__hybrid_reading, printing_houses).
narrative_ontology:constraint_victim(classical_latin_standard__hybrid_reading, scholastic_theologians).
narrative_ontology:constraint_victim(classical_latin_standard__hybrid_reading, provincial_clergy).
narrative_ontology:constraint_vindicates(classical_latin_standard__hybrid_reading, classical_corpus_grammatical_authority).
narrative_ontology:constraint_vindicates(classical_latin_standard__hybrid_reading, technical_register_autonomy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Staff the grammar schools, university humanities chairs, editorial houses, and curial style offices that decide which forms of Latin pass. Their careers, curricula, and critical editions rest on mastery of the Classical corpus; conceding that the norm is negotiable would dissolve the expertise they alone certify, while abandoning the standard would idle it. They write the textbooks, examine the candidates, return the 'barbarous' manuscript for correction, and receive the deference paid to those who can cite the exemplars from memory.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, classical_philologists, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(classical_latin_standard__hybrid_reading, classical_philologists, beneficiary).

% Operates the largest continuous Latin-writing enterprise in existence: liturgy, canon law, curial correspondence, missionary reporting. Under the hybrid settlement it keeps its accumulated sacramental, juridical, and devotional vocabulary — terms no Classical author knew — while submitting its grammar to Classical norms. Leaving Latin for the vernaculars would fracture a worldwide institution's unity of rite and record; remaining inside the standard costs periodic stylistic correction but preserves one canonical language across nations and centuries.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, ecclesiastical_curia, beneficiary,
    institutional, generational, constrained, global).

% Teach and certify the standard. They gain a stable international curriculum and a portable degree currency, and their graduates staff chanceries and faculties across borders. The cost side is real: examinations, disputation etiquette, and publication expectations obligate them to police correctness among their own members, and a professor's stylistic lapse is a public embarrassment. Shifting instruction to the vernacular means forfeiting the international student market and the exchange of scholars that the shared language makes possible.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, university_faculties, beneficiary,
    institutional, generational, constrained, continental).

% Write in the idiom built up over centuries of lecture, disputation, and glossed commentary: dense technical coinages, compressed citation habits, syntax shaped by oral teaching. The humanist standard reclassifies much of this equipment as barbarism. Their options are recasting their work in Ciceronian dress at the price of the precision their method depends on, defending the inherited idiom at the price of ridicule and blocked publication, or retreating into the orders and older faculties that still tolerate it. Their method and their Latin are one formation; neither can be surrendered without the other.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, scholastic_theologians, payer,
    organized, biographical, identity_locked, continental).

% Educated in local schools, their Latin carries the accent and habits of their vernacular. Visiting examiners, superiors, and printed style guides mark it incorrect; sermons and administrative reports are rewritten above them. Advancement is priced in compliance, and the remedy — a full humanist education — is expensive and geographically distant. The vernacular remains available for parish work, but anything that must travel beyond the diocese passes through the standard and its correctors.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, provincial_clergy, payer,
    moderate, biographical, constrained, regional).

% Sell chiefly to schools, chanceries, and curiae whose purchasing depends on correctness, so house editors normalize grammar and spelling before setting type. Standardization lowers their costs and widens their market; a firm known for barbarous texts loses the institutional trade. Capital and stock lists can move to whichever cities' conventions prevail, which makes their enforcement of the norm commercial rather than doctrinal — firm where the buyers are, lax where they are not.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, printing_houses, beneficiary,
    powerful, biographical, mobile, continental).

% Teachers and writers who hold that correct Latin is what competent contemporary speakers and writers actually produce, treating the Classical corpus as a treasury rather than a tribunal. They publish defenses of current usage and propose reforms, but they hold no examination boards, no curial office, and little press leverage; the arbitration machinery they would reform is operated by the very parties whose authority the arbitration confirms, so their objections circulate without ever reaching a decision point.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, living_latin_advocates, excluded,
    organized, generational, trapped, continental).

% Reconstruct the standard's formation, enforcement, and retreat from outside the game: collating style guides, school statutes, examination records, and editorial correspondence to establish which forms were condemned, which were grandfathered into the technical registers, and why. They bear none of the correction and collect none of the deference, which is what makes their testimony usable by every other seat in the dispute.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, philological_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(classical_latin_standard__hybrid_reading, classical_philologists).
narrative_ontology:fixing_cost_class(classical_latin_standard__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single grammatical and stylistic norm for written Latin across regions and generations, so that a text composed in one country is readable in another without translation, while exempting recognized technical registers — law, theology, natural philosophy, liturgy — from the demand to reproduce Classical vocabulary.
% TRANSFER_FUNCTION: Moves linguistic prestige, publication access, and career advancement from writers formed in local or scholastic practice toward writers formed on Classical texts; moves corrective labor — rewriting, style-policing, examination — onto those writers; concentrates editorial authority in philologically trained schoolmasters, printers, and curial officials.
% ABSENT_VOICES: Defenders of unbroken living usage — the scholastic masters whose idiom the standard condemns, and the later advocates of contemporary-use Latin — sit outside the arbitrating bodies once humanist schooling and the printing trade consolidate. They would dispute the barbarism classifications form by form, but they hold no seats on examination boards, editorial desks, or curial style offices, so their dissent never reaches the machinery that adjudicates it.
% DISAPPEARANCE_RATIONALE: If the hybrid standard vanished overnight, learned correspondence would fragment into mutually awkward regional idioms within a generation; the liturgy and canon law would lose their fixed transnational form; emerging scientific nomenclature would lose the international stability that made species names legible in every country; and the humanist school curriculum — the largest educational enterprise in Europe — would lose its organizing object. Institutions arranged around one correct Latin would rearrange around several, or around the vernaculars, at substantial cost.
% FOUNDING_PROBLEM: After the recovery and diffusion of the Classical manuscripts, learned Latin had split: scholastic practice carried a millennium of drift that the new philology judged corrupt, yet law, theology, and natural philosophy depended on post-Classical technical vocabulary that a pure return to the exemplars would discard. The founding problem was building one norm that disciplined style without amputating the technical lexicon the institutions ran on.
% FOUNDING_PROBLEM_CORROBORATION: Historians of education and of the Republic of Letters — outside the benefiting parties — document the fifteenth-century standards crisis and the deliberate compromise between textual authority and institutional need. Inside the Church, curial style offices and liturgical commissions attest that the problem of maintaining Classical grammar over an accumulating technical vocabulary remained live into the modern period. In taxonomy, the drafting history of the zoological and botanical nomenclature codes attests the same problem in scientific dress. What no outside source disputes is that the founding problem existed; what the parties dispute is whether it remains live now that the vernaculars carry nearly all learned traffic.
narrative_ontology:disappearance_verdict(classical_latin_standard__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(classical_latin_standard__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(classical_latin_standard__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(classical_latin_standard__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(classical_latin_standard__hybrid_reading, 0.46, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(classical_latin_standard__hybrid_reading_tests).
:- end_tests(classical_latin_standard__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.46 at interval end) because the arrangement couples a genuine service — a stable trans-regional written norm with protected technical registers — to an asymmetric status levy concentrated on writers formed outside the Classical curriculum; it sits well below what a reconstruction-style regime would score (all drift condemned) because the accommodation spares the technical estates. Suppression (0.45) is real but bounded: enforcement runs through examinations, editorial rejection, and curial correction rather than prohibition of alternatives, and the vernacular exit is always visible at the margin. Accessibility_collapse (0.40) is correspondingly moderate — vernaculars, tolerated medieval forms in sheltered institutions, and the accommodated registers themselves leave alternatives partly open. Resistance (0.50) reflects the durable scholastic counter-tradition, the long adherence of Iberian and German faculties to inherited idiom, and recurring living-usage advocacy. Theater_ratio (0.36) tracks the growth of Ciceronian display culture — purity contests, ornamental quotation, correction performed for audiences — atop a coordinating function that remained real throughout. The temporal series share one seven-point grid. Extractiveness humps (peaking near 1600) as enforcement infrastructure matures — the Jesuit colleges, the normalized printed textbook, curial style offices — then eases as vernaculars absorb learned traffic. Suppression_requirement follows the same arc (0.40 to 0.62 to 0.45): a genuine build-up of enforcement capacity followed by decay or scope retreat (see the enforcement_decay_or_scope_retreat omega); the story-level suppression scalar reflects the interval-end state. Theater rises monotonically: as the standard's jurisdiction shrinks, a larger share of its activity becomes performance of correctness rather than production of it.
 *
 * PERSPECTIVAL GAP:
 *   The arbiter seat and the payer seats compute differently from the same structure, and the engine derives that divergence from the declared roles and exits. From the philologist's position the arrangement is an earned order: mastery was acquired at cost, the norm is publicly inspectable in the texts, and correction is a service. From the scholastic theologian's position the same machinery is an arbitrary tribunal: the condemned forms carry distinctions his method needs, and the tribunal's judges are the parties its verdicts ennoble. Two identity locks stabilize the gap. The philologists' professional identity is fused with the standard — admitting that the norm is negotiable dissolves the expertise they alone certify — while the scholastics' identity is fused with their idiom, since disputation method and its Latin are one formation; neither seat can cheaply occupy the other's view. The printing houses, mobile and opportunistic, experience the standard as a product specification rather than a conviction, which is why their enforcement tracks markets rather than doctrine.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries derive low directionality: the philologists sit nearest the beneficiary pole (they set the rules and receive the deference the rules generate), the curia and faculties somewhat above them (genuine services received, real compliance costs borne), the printers near-symmetric-to-beneficiary (market gains, negligible submission costs). Declared victims derive high directionality: scholastic theologians sit near the full-target pole, amplified by identity_lock — they cannot rewrite their tradition's idiom without dismantling their method — and provincial_clergy just below them, constrained by the cost of the humanist remedy. Scope is continental for most seats, which modestly amplifies effective extraction for targets by making appeal or arbitrage harder; the curia's global scope cuts the other way, since no rival standard competes for its internal traffic. No directionality overrides are used: the beneficiary/victim declarations plus exit options already place every seat correctly, and the two dual-positioned agents (philologists as agenda-setter-beneficiary, faculties as beneficiary bearing enforcement labor) are handled by their role declarations and situation text rather than by overriding the derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — one authoritative norm for learned Latin after the textual disruption — is contested rather than dead: its universal mandate (all learned writing) has largely lapsed with the vernacular turn, but niche mandates survive intact in liturgy, canon law, and biological nomenclature, where the coordination function is still load-bearing. The classification guards both mislabels. Reading the arrangement as pure extraction ignores that the accommodated registers genuinely depend on the shared norm and would fragment without it; reading it as pure coordination ignores the documented status levy on non-Classically-formed writers and the arbiter rents that enforcement reproduces. The tangled_rope claim encodes exactly this coupling: coordination and extraction run through the same grammar, the same schoolroom, the same editorial desk. The R5 mismatch consumer reads founding_problem_status (contested) against disappearance_verdict (world_rearranges): no mechanical zombie flag fires, but the analysis flags the asymmetry — if the niche functions also lapse, the remaining enforcement will be identity-driven performance, and the arrangement should then be re-examined as inertial.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'This story instantiates the hybrid_reading of the classical_latin_standard kernel; would instantiating the continuity_reading or the reconstruction_reading instead yield a different beneficiary set, victim set, and epsilon for the same historical arrangement?',
    'Compare the compiled sibling stories (classical_latin_standard__continuity_reading, classical_latin_standard__reconstruction_reading): if their structural data diverge on who pays and how much, the kernel is genuinely multi-constraint and each reading must stand alone; if they converge, the kernel contest is rhetorical rather than structural.',
    'If the readings diverge structurally, cross-reading comparisons of classification are invalid and each reading''s epsilon is reading-indexed; if they converge, the three-way contest reduces to a dispute over justification rather than structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Committer-frame position: this constraint is one reading of a contested kernel, and sibling readings instantiate different constraints.').

omega_variable(
    accommodation_boundary_contest,
    'Where exactly does this reading draw the line between legitimate post-Classical technical development and condemnable barbarism — are scholastic metaphysical coinages, medieval syntactic calques, and liturgical formulas inside or outside the accommodated set?',
    'Comparative codification: inventory which post-Classical forms the curial style offices, university statutes, and later nomenclature codes admit versus correct, and test whether the admission criteria are principled (register-based) or discretionary (arbiter-based).',
    'A wide, principled admitted set pushes the arrangement toward the continuity reading''s profile (low victim count, coordination-dominated); a narrow or discretionary set pushes it toward the reconstruction reading''s profile (broad delegitimization, concentrated arbiter authority) and raises effective extraction for everyone subject to the discretion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accommodation_boundary_contest, conceptual, 'The hybrid reading''s load-bearing ambiguity: the legitimacy boundary for post-Classical forms.').

omega_variable(
    enforcement_decay_or_scope_retreat,
    'Is the post-1650 decline in required suppression enforcement decay (the correction machinery eroding) or scope retreat (Latin''s share of learned traffic shrinking, so less enforcement is needed for the same effect)?',
    'Normalize enforcement intensity per unit of Latin output: school inspection records, editorial correction rates, and examination requirements measured against the volume of Latin published and taught per decade.',
    'Enforcement decay predicts drift toward inertial, theatrically maintained persistence; scope retreat predicts a stable niche arrangement whose coordination function remains genuine where Latin survives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_decay_or_scope_retreat, empirical, 'Whether the falling suppression series reflects machinery erosion or shrinking jurisdiction.').

omega_variable(
    arbiter_identity_fusion,
    'How much of the standard''s persistence after its universal mandate lapsed depends on the arbiters'' professional identity — careers, curricula, and self-concept constituted by Ciceronian mastery — rather than on any remaining coordination need?',
    'Observe arbitral behavior where alternative credentials became available: whether schoolmasters and curial officials relaxed enforcement once vernacular polish carried equivalent career signal, or defended the standard regardless of demand.',
    'High identity dependence predicts performative maintenance and eventual piton-like inertia in the residual domains; low identity dependence predicts clean retirement of the standard where its function ends.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(arbiter_identity_fusion, empirical, 'Identity-lock contribution to the standard''s persistence independent of function.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(classical_latin_standard__hybrid_reading, 1450, 1750).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clas_tr_t1450, classical_latin_standard__hybrid_reading, theater_ratio, 1450, 0.12).
narrative_ontology:measurement(clas_tr_t1500, classical_latin_standard__hybrid_reading, theater_ratio, 1500, 0.18).
narrative_ontology:measurement(clas_tr_t1550, classical_latin_standard__hybrid_reading, theater_ratio, 1550, 0.26).
narrative_ontology:measurement(clas_tr_t1600, classical_latin_standard__hybrid_reading, theater_ratio, 1600, 0.3).
narrative_ontology:measurement(clas_tr_t1650, classical_latin_standard__hybrid_reading, theater_ratio, 1650, 0.33).
narrative_ontology:measurement(clas_tr_t1700, classical_latin_standard__hybrid_reading, theater_ratio, 1700, 0.35).
narrative_ontology:measurement(clas_tr_t1750, classical_latin_standard__hybrid_reading, theater_ratio, 1750, 0.36).

% Extraction over time
narrative_ontology:measurement(clas_be_t1450, classical_latin_standard__hybrid_reading, base_extractiveness, 1450, 0.38).
narrative_ontology:measurement(clas_be_t1500, classical_latin_standard__hybrid_reading, base_extractiveness, 1500, 0.44).
narrative_ontology:measurement(clas_be_t1550, classical_latin_standard__hybrid_reading, base_extractiveness, 1550, 0.5).
narrative_ontology:measurement(clas_be_t1600, classical_latin_standard__hybrid_reading, base_extractiveness, 1600, 0.52).
narrative_ontology:measurement(clas_be_t1650, classical_latin_standard__hybrid_reading, base_extractiveness, 1650, 0.5).
narrative_ontology:measurement(clas_be_t1700, classical_latin_standard__hybrid_reading, base_extractiveness, 1700, 0.47).
narrative_ontology:measurement(clas_be_t1750, classical_latin_standard__hybrid_reading, base_extractiveness, 1750, 0.46).

% Suppression requirement over time
narrative_ontology:measurement(clas_su_t1450, classical_latin_standard__hybrid_reading, suppression_requirement, 1450, 0.4).
narrative_ontology:measurement(clas_su_t1500, classical_latin_standard__hybrid_reading, suppression_requirement, 1500, 0.52).
narrative_ontology:measurement(clas_su_t1550, classical_latin_standard__hybrid_reading, suppression_requirement, 1550, 0.62).
narrative_ontology:measurement(clas_su_t1600, classical_latin_standard__hybrid_reading, suppression_requirement, 1600, 0.6).
narrative_ontology:measurement(clas_su_t1650, classical_latin_standard__hybrid_reading, suppression_requirement, 1650, 0.54).
narrative_ontology:measurement(clas_su_t1700, classical_latin_standard__hybrid_reading, suppression_requirement, 1700, 0.49).
narrative_ontology:measurement(clas_su_t1750, classical_latin_standard__hybrid_reading, suppression_requirement, 1750, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(classical_latin_standard__hybrid_reading, information_standard).
narrative_ontology:affects_constraint(classical_latin_standard__hybrid_reading, classical_latin_standard__continuity_reading).
narrative_ontology:affects_constraint(classical_latin_standard__hybrid_reading, classical_latin_standard__reconstruction_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'correct Latin' decomposes into three structurally distinct claims per the epsilon-invariance principle. continuity_reading (upstream: the pre-humanist default, authority in practice) predates the textual recovery; reconstruction_reading (downstream: authority in archaeological expertise) is enabled by it; hybrid_reading mediates between them and is the arrangement that actually governed European learned life from roughly 1450 to 1750. Each member carries its own epsilon, beneficiary set, and victim set: continuity minimizes the victim set, reconstruction maximizes it, hybrid splits the difference through the accommodation boundary. Every family member links the others through affects_constraints; the hybrid story additionally documents the boundary ambiguity (accommodation_boundary_contest omega) that separates it from both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
