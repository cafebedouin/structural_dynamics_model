% ============================================================================
% CONSTRAINT STORY: us_constitution_1787__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_1787__originalist_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: us_constitution_1787__originalist_reading
 *   human_readable: Constitutional Originalism: Meaning Fixed at 1787 Ratification
 *   domain: constitutional_law/legal_theory/political_philosophy
 *
 * SUMMARY:
 *   Constitutional originalism holds that the Constitution's meaning is fixed
 *   at the moment of ratification in 1787 and that judges must interpret it
 *   by reference to the framers' intent and the original public meaning of
 *   the text as understood by the ratifying generation. This reading emerged
 *   as a powerful jurisprudential movement in the late 20th century, claiming
 *   to constrain judicial discretion by tying meaning to historical fact
 *   rather than evolving values. Originalism is claimed as a constraint-type
 *   coordinate system (a tangled rope: genuine coordination function around a
 *   shared interpretive methodology, coupled with asymmetric extraction that
 *   forecloses certain rights claims and benefits particular political
 *   factions). The originalist reading coexists with the living-constitution
 *   reading (which treats meaning as evolving) and the positivist reading
 *   (which grounds meaning in amended text and democratic process, not
 *   historical intent). This story instantiates ONLY the originalist reading
 *   as a coherent constraint with its own ε, structural asymmetries, and
 *   beneficiary/victim set.
 *
 * KEY AGENTS:
 *   - Originalist judiciary: Sets and enforces the interpretive standard; controls what counts as legitimate constitutional argument in courtrooms
 *   - Conservative political factions: Benefit from originalism's preservation of 1787 federalism limits and property arrangements; fund appointments and scholarship
 *   - Claimants of unenumerated rights: Pay the cost of a doctrine that forecloses rights claims not rooted in founding-era practice (privacy, bodily autonomy, welfare rights)
 *   - Marginalized groups excluded from 1787 franchise: Bear the cost of a doctrine that treats their historical exclusion as the constitutional baseline
 *   - Living-constitution advocates: Largely excluded from originalist-dominated courts during the doctrine's ascendancy; their reading remains live in alternative institutional contexts
 *   - Legal academia: Produces the intellectual apparatus legitimizing and challenging originalism; trains the next generation of interpreters
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_1787__originalist_reading, 0.62).
domain_priors:suppression_score(us_constitution_1787__originalist_reading, 0.71).
domain_priors:theater_ratio(us_constitution_1787__originalist_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_1787__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_1787__originalist_reading, "Constitutional Originalism: Meaning Fixed at 1787 Ratification").
narrative_ontology:topic_domain(us_constitution_1787__originalist_reading, "constitutional_law/legal_theory/political_philosophy").

domain_priors:requires_active_enforcement(us_constitution_1787__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_1787__originalist_reading, '38ca18ce-d3d8-4972-9526-9a34be179e4a').
narrative_ontology:cs_kernel_codification('38ca18ce-d3d8-4972-9526-9a34be179e4a', fixed_text).
narrative_ontology:cs_authority_grounding('38ca18ce-d3d8-4972-9526-9a34be179e4a', lineage).
narrative_ontology:cs_interpretation_layer_present('38ca18ce-d3d8-4972-9526-9a34be179e4a').
narrative_ontology:cs_reading_relation('38ca18ce-d3d8-4972-9526-9a34be179e4a', us_constitution_1787__living_reading, coexists_with).
narrative_ontology:cs_reading_relation('38ca18ce-d3d8-4972-9526-9a34be179e4a', us_constitution_1787__positivist_reading, influences).
narrative_ontology:cs_axiom('38ca18ce-d3d8-4972-9526-9a34be179e4a', foundational, constitutional_meaning_temporally_fixed).
narrative_ontology:cs_axiom_status(constitutional_meaning_temporally_fixed, holdable).
narrative_ontology:cs_axiom_grounding('38ca18ce-d3d8-4972-9526-9a34be179e4a', constitutional_meaning_temporally_fixed, deontological).
narrative_ontology:cs_axiom('38ca18ce-d3d8-4972-9526-9a34be179e4a', foundational, framers_intent_is_authoritative_source).
narrative_ontology:cs_axiom_status(framers_intent_is_authoritative_source, holdable).
narrative_ontology:cs_axiom_grounding('38ca18ce-d3d8-4972-9526-9a34be179e4a', framers_intent_is_authoritative_source, empirically_contingent).
narrative_ontology:cs_reference_frame('38ca18ce-d3d8-4972-9526-9a34be179e4a', framers_intent_binding_meaning_fixed_at_ratification).
narrative_ontology:cs_drift_state('38ca18ce-d3d8-4972-9526-9a34be179e4a', contemporary_judicial_practice_2025, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('38ca18ce-d3d8-4972-9526-9a34be179e4a', '').
narrative_ontology:cs_kernel_id(us_constitution_1787__originalist_reading, us_constitution_1787).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_1787__originalist_reading, originalist_judiciary).
narrative_ontology:constraint_beneficiary(us_constitution_1787__originalist_reading, conservative_political_factions).
narrative_ontology:constraint_beneficiary(us_constitution_1787__originalist_reading, property_interests_favored_in_1787).
narrative_ontology:constraint_victim(us_constitution_1787__originalist_reading, claimants_of_unenumerated_rights).
narrative_ontology:constraint_victim(us_constitution_1787__originalist_reading, marginalized_groups_excluded_from_1787_franchise).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(us_constitution_1787__originalist_reading, state_legislatures).
narrative_ontology:constraint_victim(us_constitution_1787__originalist_reading, state_legislatures).
narrative_ontology:constraint_vindicates(us_constitution_1787__originalist_reading, framers_intent_is_determinable).
narrative_ontology:constraint_vindicates(us_constitution_1787__originalist_reading, historical_practice_constrains_meaning).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Federal judges who interpret the Constitution by reference to its original public meaning at ratification. They claim fidelity to law and constraint on judicial discretion. They control what counts as legitimate constitutional reasoning in their courtrooms and appellate decisions. Their interpretive stance limits which arguments succeed; plaintiffs seeking rights not textually enumerated or rooted in 1787 practice face near-zero success rates.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, originalist_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Political movements and parties that align originalism with their policy agenda (e.g., limiting federal regulatory scope, restricting abortion access, preserving property and commerce rules consistent with 1787). They benefit from a judicial doctrine that treats 18th-century property and federalism arrangements as constitutionally binding, foreclosing certain modern regulatory moves. They fund originalist legal scholarship and judicial appointments.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, conservative_political_factions, beneficiary,
    organized, generational, constrained, national).

% Individuals and groups claiming rights not explicitly listed in the 1787 text or rooted in founding-era practice (e.g., privacy, same-sex marriage, bodily autonomy in novel contexts, welfare rights). Under originalism, their constitutional claims face the evidentiary burden of proving the framers intended or the ratifying public understood their claimed right. Many such claims cannot clear this bar and are deemed outside the constitutional boundary. Their exit is constrained by citizenship and the constitutional order itself.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, claimants_of_unenumerated_rights, payer,
    powerless, biographical, identity_locked, national).

% African Americans, women, indigenous peoples, and other groups who were systematically excluded from the founding-era political and legal community. Originalism's fidelity to 1787 meanings and practices means it defaults to the exclusionary status quo: slavery, coverture, denial of suffrage, and territorial dispossession are constitutional under the constraint's own reasoning unless later amendments explicitly repudiate them. They bear the cost of a doctrine that treats their exclusion as the legitimate baseline.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, marginalized_groups_excluded_from_1787_franchise, payer,
    powerless, generational, trapped, national).

% Judges, scholars, and advocates who argue constitutional meaning evolves with social understanding and that the text should be read as an aspirational framework adapting to new contexts. They are largely excluded from federal appellate and Supreme Court appointments in originalist-dominated eras and from successful constitutional arguments in those courts. They argue originalism forecloses necessary social adaptation but lack institutional position to enforce their reading during originalist ascendancy.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, living_constitution_advocates, excluded,
    organized, generational, constrained, national).

% Scholars and law professors who debate the epistemology of originalism: whether framers' intent is recoverable, whether original public meaning is ascertainable, whether fidelity to 1787 meanings is coherent or fabricated. They produce the intellectual apparatus that legitimizes or challenges the doctrine and train the next generation of lawyers and judges. Their work shapes the constraint but does not directly enforce it.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, legal_academia, observer,
    moderate, biographical, mobile, national).

% State-level legislatures that are constrained by federal courts' originalist interpretation of the Fourteenth Amendment, Commerce Clause, and other provisions. Some benefit (those whose regulations align with 1787-era federalism limits) while others pay (those whose social regulations are struck down as violating unamended text). Their ability to innovate legislatively depends on whether the courts' originalist reading permits the regulation.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, state_legislatures, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_1787__originalist_reading, state_legislatures, beneficiary).

% Historians, archivists, and textual scholars who produce evidence of what the framers and ratifying public thought. Originalism depends on their work being reliable and determinate, yet their scholarship often shows ambiguity, inconsistency, and gaps in the historical record. They are called as expert witnesses and their findings are cited by courts, but disagreements among historians are weaponized to support preferred outcomes.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, historical_evidence_specialists, observer,
    moderate, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_1787__originalist_reading, originalist_judiciary).
narrative_ontology:fixing_cost_class(us_constitution_1787__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable interpretive framework for constitutional law: by anchoring meaning to a fixed point (1787 ratification), it claims to constrain judicial discretion, prevent ad hoc rewriting of the Constitution, and make constitutional meaning determinate rather than subject to each generation's preferences. Coordinates the federal judiciary around a shared epistemology for legitimate constitutional argument.
% TRANSFER_FUNCTION: Transfers legitimacy and legal standing from claimants seeking rights grounded in evolving social understanding to those whose rights are rooted in 1787 text and practice. Moves institutional power from living-constitution judges (who claim broad discretion) to originalist judges (who claim constraint by historical fact). Moves cultural authority from rights advocates to constitutional historians and originalist scholars.
% ABSENT_VOICES: Enslaved people, women, indigenous peoples, and other groups excluded from the 1787 founding are not present to contest what their exclusion from the original public meaning entails. Living-constitution judges are substantially excluded from federal appellate appointments in originalist eras and cannot argue their reading successfully in courts they do not staff. Rights claimants and social movements that depend on evolving constitutional interpretation lack institutional voice in originalist-dominated courts.
% DISAPPEARANCE_RATIONALE: If originalism as a binding interpretive constraint vanished, constitutional litigation would immediately shift to competing frameworks (living constitution, textualism without historical fixity, purposivism). Rights claims currently foreclosed by originalism's historical evidentiary burden would become arguable. Federal-state regulatory relations would realign. The institutional composition of courts and the strategic litigation campaigns of advocacy groups would change direction. Conservative jurisprudence would lose its primary legitimating doctrine.
% FOUNDING_PROBLEM: In the late 20th century, federal judges were accused of rewriting the Constitution to match their policy preferences, treating the document as a blank check for judicial discretion. Originalism emerged as a doctrine claiming to constrain judges by tying constitutional meaning to historical fact, not evolving values. The founding problem was: how can we prevent judges from imposing their own preferred social policies under the guise of constitutional interpretation?
% FOUNDING_PROBLEM_CORROBORATION: Originalists attest the problem remains live, citing examples of decisions they view as activist. Critics (legal scholars, judges, rights advocates outside the originalist movement) attest the founding problem was overstated and that originalism itself is a discretionary tool that substitutes historical contingency for policy preference: historians disagree on original meaning, gaps in the historical record are filled by assumption, and the selection of what historical sources to credit embeds unstated preferences. No consensus outside the originalist movement that the founding problem persists or that originalism solves it.
narrative_ontology:disappearance_verdict(us_constitution_1787__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_1787__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_1787__originalist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(us_constitution_1787__originalist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_1787__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_1787__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_1787__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.31 (1970: originalism still marginal) to 0.62 (2025: dominant in federal judiciary and Supreme Court). The metric tracks increasing institutional power: as originalist judges populate appellate benches and especially the Supreme Court (appointments accelerating from 2000 onward), the doctrine's capacity to foreclose rights claims and enforce preferred outcomes increases. Suppression also rises (0.38 to 0.71): the constraint requires actively suppressing alternative interpretive frameworks and excluding living-constitution judges from high courts. Theater ratio rises too (0.22 to 0.48): the rationale that originalism constrains judges via historical fidelity becomes increasingly performative as critics document selective use of historical sources, gaps bridged by assumption, and outcomes that align suspiciously with the appointing faction's preferences. The claim is tangled_rope because originalism does provide genuine coordination (shared epistemic standard for constitutional argument) AND requires enforcement to suppress alternatives and foreclose certain rights claims. The beneficiary set is narrow (originalist judges who gain interpretive authority, conservative factions whose policy preferences align with 1787 federalism) while the victim set is broad (rights claimants, marginalized groups). Theater rising while extraction also rises is the signal that performance (fidelity to history) is replacing function (constraint on discretion).
 *
 * PERSPECTIVAL GAP:
 *   An originalist judge experiences this constraint as a faithful, limiting doctrine that prevents her from imposing her own values — she experiences low directionality (beneficiary of constraint on discretion). A living-constitution judge excluded from originalist-majority courts experiences the same constraint as enforced exclusion (high directionality, near-target). A claimant seeking a right not rooted in 1787 practice experiences extreme directionality (nearly pure target: the doctrine structurally forecloses her claim). Conservative legislators whose powers are preserved by originalism's federalism reading experience low directionality (beneficiary). Progressive legislators whose regulatory authority is constrained experience higher directionality (payer). The engine should compute these asymmetries from the beneficiary/victim declarations and the exit_options per seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Originalist judiciary: Beneficiary (low d). Conservative political factions: Beneficiary (low d, organized). Claimants of unenumerated rights: Victim (high d); their exit is identity_locked because constitutional status is bound up with citizenship and they cannot leave the constitutional order. Marginalized groups excluded from 1787: Victim (highest d, trapped; they are most constrained and least able to exit the constitutional system). Living-constitution advocates: Excluded (high d when excluded from courts, beneficiary when staffing alternative forums); we treat them as partially excluded due to the doctrine's institutional dominance. The directionality dynamics show the asymmetry that defines tangled_rope: genuine coordination around a shared interpretive method, coupled with enforcement that benefits some parties and targets others.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (constraining judges to prevent discretionary rewriting) was live in 1970s but is contested now. Critics argue originalism has become its own discretionary tool: historical sources are selected to confirm preferred outcomes, gaps in the record are filled by assumption, and 'original meaning' is constructed rather than discovered. Yet the institutional position of originalism has strengthened, not weakened, suggesting the constraint persists by inertia and appointment strategy rather than by solving its founding problem. Theater ratio rising while extraction rises (not falls) is diagnostic: the constraint's performance as historical fidelity becomes less credible even as its extractive power grows. This is a constraint whose founding mandate has degraded but whose institutional force has hardened — a slow-motion shift from genuine coordination to performance covering extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    framers_intent_determinacy,
    'Is framers'' intent recoverable from historical sources with sufficient determinacy to ground constitutional meaning?',
    'Historiographical analysis comparing historical records of the 1787 Convention and ratification debates. Cross-examination of originalists'' use of primary sources against historians'' assessments of what the sources actually establish. Documentation of gaps, ambiguities, and alternative interpretations in the historical record.',
    'If framers'' intent is indeterminate (many gaps, serious disagreements among historians, no unified intent), originalism''s claim to constrain judges by historical fact collapses — judges would be filling gaps by assumption (discretion in disguise). If intent is determinate, originalism''s epistemic foundation holds and constrains judges meaningfully.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framers_intent_determinacy, empirical, 'Whether the historical record provides determinate evidence of framers'' intent').

omega_variable(
    original_public_meaning_vs_framer_intent,
    'Should originalism track the framers'' subjective intent, the original public meaning of the text as the ratifying public would have understood it, or some hybrid?',
    'Doctrinal development: judges and scholars settle which version of originalism is authoritative. Institutional coherence: which version produces more determinate outcomes and fewer disagreements among originalists.',
    'Intent-originalism and public-meaning-originalism often produce different results (e.g., on federal commerce power, Second Amendment scope). The internal debate within originalism about which version is correct is itself a site of discretion — different answers favor different beneficiary/victim asymmetries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_public_meaning_vs_framer_intent, conceptual, 'Ambiguity within originalism about which historical standard governs meaning').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.71) structural (external barriers to alternative interpretations) or internalized (judges who have absorbed originalism''s legitimacy claims and silence alternatives internally)?',
    'Post-institutional exit trajectory: if originalist judges who retire or change positions revert to living-constitution reasoning, suppression is partly internalized; if they persist in originalist reasoning after leaving the bench, the suppression may be more structural (appointment filtering, institutional incentives).',
    'If suppression is primarily internalized, courts with different appointment demographics could rapidly shift interpretation; if structural, the barriers to alternative readings are more durable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of alternative readings is structural or internalized').

omega_variable(
    false_summit_natural_law_ambiguity,
    'Is originalism a discovered natural law of constitutional meaning (the Founders'' intent exists in history as a brute fact), or is it a constructed constraint that benefits particular parties while claiming naturality?',
    'Comparison with other readings: if the living-constitution reading and the positivist reading describe the same constitutional domain with equal empirical adequacy, then ''original meaning'' is not a discovered fact but a choice among frameworks. If one reading is markedly less empirically adequate, that signals a genuine natural law. The rising theater_ratio and the beneficiary/victim asymmetry suggest constructed framing.',
    'If originalism is discovered law (natural), its extraction is the price of accessing constitutional meaning itself. If constructed, its extraction is a political choice benefiting conservative factions and marginalizing certain rights claims.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(false_summit_natural_law_ambiguity, conceptual, 'Whether originalism is a natural law of constitutional meaning or a constructed doctrine benefiting particular parties').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_1787__originalist_reading, 1970, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1970, us_constitution_1787__originalist_reading, theater_ratio, 1970, 0.22).
narrative_ontology:measurement_basis(us_c_tr_t1970, observed).
narrative_ontology:measurement(us_c_tr_t1985, us_constitution_1787__originalist_reading, theater_ratio, 1985, 0.31).
narrative_ontology:measurement_basis(us_c_tr_t1985, observed).
narrative_ontology:measurement(us_c_tr_t2000, us_constitution_1787__originalist_reading, theater_ratio, 2000, 0.38).
narrative_ontology:measurement_basis(us_c_tr_t2000, observed).
narrative_ontology:measurement(us_c_tr_t2015, us_constitution_1787__originalist_reading, theater_ratio, 2015, 0.45).
narrative_ontology:measurement_basis(us_c_tr_t2015, observed).
narrative_ontology:measurement(us_c_tr_t2025, us_constitution_1787__originalist_reading, theater_ratio, 2025, 0.48).
narrative_ontology:measurement_basis(us_c_tr_t2025, observed).
narrative_ontology:measurement(us_c_tr_t2030, us_constitution_1787__originalist_reading, theater_ratio, 2030, 0.51).
narrative_ontology:measurement_basis(us_c_tr_t2030, projected).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1970, us_constitution_1787__originalist_reading, base_extractiveness, 1970, 0.31).
narrative_ontology:measurement_basis(us_c_be_t1970, observed).
narrative_ontology:measurement(us_c_be_t1985, us_constitution_1787__originalist_reading, base_extractiveness, 1985, 0.42).
narrative_ontology:measurement_basis(us_c_be_t1985, observed).
narrative_ontology:measurement(us_c_be_t2000, us_constitution_1787__originalist_reading, base_extractiveness, 2000, 0.51).
narrative_ontology:measurement_basis(us_c_be_t2000, observed).
narrative_ontology:measurement(us_c_be_t2015, us_constitution_1787__originalist_reading, base_extractiveness, 2015, 0.59).
narrative_ontology:measurement_basis(us_c_be_t2015, observed).
narrative_ontology:measurement(us_c_be_t2025, us_constitution_1787__originalist_reading, base_extractiveness, 2025, 0.62).
narrative_ontology:measurement_basis(us_c_be_t2025, observed).
narrative_ontology:measurement(us_c_be_t2030, us_constitution_1787__originalist_reading, base_extractiveness, 2030, 0.62).
narrative_ontology:measurement_basis(us_c_be_t2030, projected).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1970, us_constitution_1787__originalist_reading, suppression_requirement, 1970, 0.38).
narrative_ontology:measurement_basis(us_c_su_t1970, observed).
narrative_ontology:measurement(us_c_su_t1985, us_constitution_1787__originalist_reading, suppression_requirement, 1985, 0.48).
narrative_ontology:measurement_basis(us_c_su_t1985, observed).
narrative_ontology:measurement(us_c_su_t2000, us_constitution_1787__originalist_reading, suppression_requirement, 2000, 0.58).
narrative_ontology:measurement_basis(us_c_su_t2000, observed).
narrative_ontology:measurement(us_c_su_t2015, us_constitution_1787__originalist_reading, suppression_requirement, 2015, 0.66).
narrative_ontology:measurement_basis(us_c_su_t2015, observed).
narrative_ontology:measurement(us_c_su_t2025, us_constitution_1787__originalist_reading, suppression_requirement, 2025, 0.71).
narrative_ontology:measurement_basis(us_c_su_t2025, observed).
narrative_ontology:measurement(us_c_su_t2030, us_constitution_1787__originalist_reading, suppression_requirement, 2030, 0.72).
narrative_ontology:measurement_basis(us_c_su_t2030, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_1787__originalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_1787__originalist_reading, 0.12).
narrative_ontology:affects_constraint(us_constitution_1787__originalist_reading, us_constitution_1787__living_reading).
narrative_ontology:affects_constraint(us_constitution_1787__originalist_reading, us_constitution_1787__positivist_reading).

% DUAL FORMULATION NOTE:
% Constitutional meaning is a contested kernel. The originalist reading ('meaning fixed at 1787 ratification') differs structurally from the living-constitution reading ('meaning evolves') and the positivist reading ('meaning is amended text plus democratic amendment'). Each reading has its own ε, beneficiary/victim structure, and institutional location. All three are linked via network.affects_constraints because they are competing interpretations of the same constitutional system. The three readings together form a constraint family decomposing 'constitutional meaning' into three structurally distinct claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
