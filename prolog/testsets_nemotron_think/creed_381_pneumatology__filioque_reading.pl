% ============================================================================
% CONSTRAINT STORY: creed_381_pneumatology__filioque_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_creed_381_pneumatology__filioque_reading, []).

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
 *   constraint_id: creed_381_pneumatology__filioque_reading
 *   human_readable: Filioque Dogma: Double Procession Defined by Magisterial Authority
 *   domain: historical_theology/ecclesiastical_authority
 *
 * SUMMARY:
 *   The Filioque reading asserts that the Holy Spirit proceeds eternally from
 *   the Father AND the Son (Filioque), and that the papal/conciliar
 *   magisterium possesses authority to define this as de fide dogma,
 *   clarifying what was implicit in the 381 Creed. Originating as a Spanish
 *   anti-Arian addition (589), adopted by Charlemagne's court (809), imposed
 *   on Rome (1014), dogmatized at Florence (1439) and Vatican I (1870). The
 *   constraint coordinates Western Trinitarian unity but extracts theological
 *   autonomy from Eastern churches, making papal definition the condition of
 *   orthodoxy. High extractiveness (0.78) reflects the structural
 *   reconfiguration of ecclesial polity: the conciliar-reception model is
 *   replaced by magisterial-definition model. Suppression (0.82) reflects
 *   anathemas, forced unions, and the ongoing canonical requirement of
 *   Filioque assent. Theater (0.45) reflects genuine theological
 *   argumentation (Augustine, Aquinas, Maximus) that serves the centralized
 *   authority structure. The claim/metric gap: the reading CLAIMS rope
 *   (coordination of orthodoxy) while metrics describe tangled_rope
 *   (coordination + asymmetric extraction of Eastern autonomy).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(creed_381_pneumatology__filioque_reading, 0.78).
domain_priors:suppression_score(creed_381_pneumatology__filioque_reading, 0.82).
domain_priors:theater_ratio(creed_381_pneumatology__filioque_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(creed_381_pneumatology__filioque_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(creed_381_pneumatology__filioque_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(creed_381_pneumatology__filioque_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(creed_381_pneumatology__filioque_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(creed_381_pneumatology__filioque_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(creed_381_pneumatology__filioque_reading, tangled_rope).
narrative_ontology:human_readable(creed_381_pneumatology__filioque_reading, "Filioque Dogma: Double Procession Defined by Magisterial Authority").
narrative_ontology:topic_domain(creed_381_pneumatology__filioque_reading, "historical_theology/ecclesiastical_authority").

domain_priors:requires_active_enforcement(creed_381_pneumatology__filioque_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(creed_381_pneumatology__filioque_reading, '3d7e87e9-8d1e-41b7-9249-b520f8c4333a').
narrative_ontology:cs_kernel_codification('3d7e87e9-8d1e-41b7-9249-b520f8c4333a', formalized).
narrative_ontology:cs_authority_grounding('3d7e87e9-8d1e-41b7-9249-b520f8c4333a', lineage).
narrative_ontology:cs_interpretation_layer_present('3d7e87e9-8d1e-41b7-9249-b520f8c4333a').
narrative_ontology:cs_reading_relation('3d7e87e9-8d1e-41b7-9249-b520f8c4333a', creed_381_pneumatology__monoprocession_reading, forecloses).
narrative_ontology:cs_reading_relation('3d7e87e9-8d1e-41b7-9249-b520f8c4333a', creed_381_pneumatology__ecumenical_reunion_reading, forecloses).
narrative_ontology:cs_axiom('3d7e87e9-8d1e-41b7-9249-b520f8c4333a', foundational, filioque_de_fide_dogma).
narrative_ontology:cs_axiom_status(filioque_de_fide_dogma, holdable).
narrative_ontology:cs_axiom_grounding('3d7e87e9-8d1e-41b7-9249-b520f8c4333a', filioque_de_fide_dogma, theological).
narrative_ontology:cs_axiom('3d7e87e9-8d1e-41b7-9249-b520f8c4333a', foundational, magisterial_authority_clarifies_implicit_doctrine).
narrative_ontology:cs_axiom_status(magisterial_authority_clarifies_implicit_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('3d7e87e9-8d1e-41b7-9249-b520f8c4333a', magisterial_authority_clarifies_implicit_doctrine, conventional).
narrative_ontology:cs_reference_frame('3d7e87e9-8d1e-41b7-9249-b520f8c4333a', creed_381_roman_reception).
narrative_ontology:cs_drift_state('3d7e87e9-8d1e-41b7-9249-b520f8c4333a', post_vatican_ii_ecumenical_dialogue, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3d7e87e9-8d1e-41b7-9249-b520f8c4333a', '').
narrative_ontology:cs_kernel_id(creed_381_pneumatology__filioque_reading, creed_381_pneumatology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(creed_381_pneumatology__filioque_reading, papal_see).
narrative_ontology:constraint_beneficiary(creed_381_pneumatology__filioque_reading, latin_theologians).
narrative_ontology:constraint_victim(creed_381_pneumatology__filioque_reading, eastern_churches).
narrative_ontology:constraint_victim(creed_381_pneumatology__filioque_reading, greek_theologians).
narrative_ontology:constraint_vindicates(creed_381_pneumatology__filioque_reading, papal_primacy_in_doctrinal_definition).
narrative_ontology:constraint_vindicates(creed_381_pneumatology__filioque_reading, development_of_doctrine_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercises magisterial authority to define the Filioque as de fide dogma (Florence 1439, Vatican I 1870). Collects centralized doctrinal control and the structural benefit of being the final arbiter of Trinitarian orthodoxy. Exit from this role would require renunciation of papal primacy claims — structurally unavailable within the reading's own framework.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, papal_see, agenda_setter,
    institutional, civilizational, arbitrage, universal).
narrative_ontology:stakeholder_secondary_role(creed_381_pneumatology__filioque_reading, papal_see, beneficiary).

% Their theological tradition (Augustine, Aquinas, medieval Scholasticism) is validated as normative. They gain career structures, magisterial posts, and intellectual authority within the Latin communion. Exit means leaving the Latin theological ecosystem — possible but professionally costly.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, latin_theologians, beneficiary,
    organized, generational, constrained, continental).

% Their theological autonomy (monoprocession as conciliar dogma of 381) is overridden by a unilateral Western definition. They bear the cost of schism (1054), repeated rejection of union councils (Lyons II 1274, Florence 1439), and ongoing marginalization in ecumenical dialogue. Their identity is fused with the 381 Creed unchanged; exit from this identity would dissolve their self-understanding as the Orthodox Church.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, eastern_churches, payer,
    organized, civilizational, identity_locked, continental).

% Their patristic tradition (Cappadocians, Maximus Confessor, Palamas) is declared theologically deficient by the Filioque definition. They are excluded from magisterial structures that define orthodoxy. Their theological vocabulary (monoprocession, monarchy of the Father) is treated as error rather than complementary expression. Exit from this framing requires abandoning the patristic synthesis that constitutes their theological identity.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, greek_theologians, payer,
    moderate, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(creed_381_pneumatology__filioque_reading, greek_theologians, excluded).

% The seven ecumenical councils recognized by both East and West. The 381 Creed (Constantinople I) contains no Filioque. The Filioque reading claims authority to clarify what these councils left implicit; the councils themselves are silent on double procession. They stand as the historical reference point whose reception diverges.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, ecumenical_councils_381_787, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(creed_381_pneumatology__filioque_reading, ecumenical_councils_381_787).

% Official Catholic-Orthodox dialogue (since 1980) and Anglican-Orthodox dialogue. They analyze the Filioque as a linguistic-conceptual difference rather than doctrinal contradiction. They have no authority to bind either communion; their role is analytical and advisory. Exit is always available but would abandon the reconciliation project.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, contemporary_ecumenical_dialogue, observer,
    institutional, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(creed_381_pneumatology__filioque_reading, papal_see).
narrative_ontology:fixing_cost_class(creed_381_pneumatology__filioque_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, authoritative Trinitarian doctrine for the Western communion, preventing doctrinal fragmentation and securing communion unity under a defined formula. The Filioque functions as a shibboleth of Latin orthodoxy and a boundary marker of papal teaching authority.
% TRANSFER_FUNCTION: Moves doctrinal authority from the conciliar-reception model (East: 381 Creed received by the whole Church) to the magisterial-definition model (West: Pope/councils define doctrine binding all). Transfers theological autonomy from Eastern patriarchates to the Roman See. Transfers the cost of schism and ongoing division to the Eastern churches.
% ABSENT_VOICES: The pre-schism undivided Church (the Fathers of 381 themselves) — they cannot testify whether the Filioque was implicit in their formula. The laity of both communions — never consulted on doctrinal definitions. The Oriental Orthodox churches — excluded from both the 381 Council and the later Filioque controversies, yet affected by the ecclesial fragmentation.
% DISAPPEARANCE_RATIONALE: If the Filioque dogma and its magisterial enforcement vanished overnight: the 1054 schism's doctrinal core would dissolve; Eastern churches would no longer face a dogmatic barrier to communion with Rome; the papal claim to define doctrine unilaterally would lose its flagship case; Latin theological curricula would require restructuring; ecumenical dialogue would shift from 'overcoming a dogmatic difference' to 'reconciling liturgical-theological expressions.' The ecclesial polity of both communions would reorganize.
% FOUNDING_PROBLEM: The Latin West faced theological confusion about the Spirit's procession (Arianism, Macedonianism) and needed a clear formula to safeguard the Spirit's full divinity against subordinationist readings. The Filioque emerged in Spain (c. 589) as an anti-Arian safeguard, later adopted as a universal Latin creedal addition.
% FOUNDING_PROBLEM_CORROBORATION: Latin patristic sources (Isidore of Seville, Council of Toledo 589) attest the anti-Arian intent. Eastern sources (Maximus Confessor, 7th c.) attest the Filioque was unknown in the East and introduced without ecumenical consent. Modern historians (Siecienski, Fotopoulos) confirm the founding problem was real but the solution exceeded the problem — the Filioque became a claim of magisterial authority, not merely an anti-Arian formula. No source outside the Latin beneficiary tradition corroborates that the 381 Fathers implicitly taught double procession.
narrative_ontology:disappearance_verdict(creed_381_pneumatology__filioque_reading, world_rearranges).
narrative_ontology:founding_problem_status(creed_381_pneumatology__filioque_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(creed_381_pneumatology__filioque_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(creed_381_pneumatology__filioque_reading, 'none', 1).
narrative_ontology:epsilon_provenance(creed_381_pneumatology__filioque_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(creed_381_pneumatology__filioque_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(creed_381_pneumatology__filioque_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(creed_381_pneumatology__filioque_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because the constraint reconfigures the entire ecclesial authority structure: doctrine is no longer received by the Church but defined by the magisterium. The Eastern churches pay the cost (schism, marginalization, loss of conciliar parity). Suppression is higher because the constraint's persistence depends on actively maintaining the dogmatic definition against Eastern rejection — anathemas at Florence, Vatican I's papal infallibility definition, and continued canonical requirement. Theater is moderate because the theological tradition (Augustine's De Trinitate, Aquinas, Latin conciliar theology) is genuine intellectual work, not mere performance, but it functions to legitimize the centralized authority. Accessibility collapse is very high (0.88) because within the Catholic communion, the Filioque is de fide — alternatives are not just discouraged but canonically excluded. Resistance remains high (0.75) because Eastern churches have never accepted the dogma, and modern ecumenical dialogue treats it as a linguistic difference, not a dogmatic necessity.
 *
 * PERSPECTIVAL GAP:
 *   From the papal_see seat: the Filioque is genuine coordination — a necessary clarification that preserves the Spirit's full divinity and manifests papal charism of truth. From eastern_churches seat: the same structure is enforced extraction — their conciliar dogma (381) unilaterally amended, their theological autonomy overridden, their communion broken. The engine computes this divergence from the structural data: papal_see has institutional power + arbitrage exit (beneficiary); eastern_churches have organized power but identity_locked exit (payer). The magisterium's authority is both the coordination mechanism and the extraction instrument.
 *
 * DIRECTIONALITY LOGIC:
 *   Papal see: full beneficiary (d ~ 0.1) — collects doctrinal control, defines orthodoxy, arbitrage exit (could theoretically renounce but framework prevents it). Latin theologians: beneficiary (d ~ 0.25) — validated tradition, career structures, constrained exit. Eastern churches: full target (d ~ 0.95) — bear schism cost, identity_locked exit (theological self-understanding fused with mono-procession). Greek theologians: target (d ~ 0.9) — tradition declared erroneous, identity_locked exit. Ecumenical councils: analytical observer (d = 0.5). Contemporary dialogue: analytical observer (d = 0.5). The identity_locked exit of Eastern actors amplifies their effective extraction — they cannot leave the constraint without dissolving their ecclesial identity.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (anti-Arian safeguard) is contested: the Filioque solved a real 6th-century Latin problem but became a 9th-century imperial tool (Charlemagne), an 11th-century schism catalyst, a 15th-century union condition, and a 19th-century infallibility showcase. The mandate (protect Spirit's divinity) atrophied; the structure (papal definition authority) persisted and expanded. The constraint prevents mislabeling: it is not pure extraction (genuine theological coordination exists in the Latin tradition) nor pure coordination (Eastern autonomy is structurally overridden). The tangled_rope classification captures the dual function: the coordination story (Trinitarian unity) and the extraction story (centralized magisterial authority) are the same structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    filioque_implicit_in_381,
    'Was the Filioque implicitly contained in the 381 Creed''s ''proceeds from the Father,'' or is it a novel theological addition?',
    'Historical-critical analysis of 4th-century pneumatology: did the Cappadocian Fathers (Basil, Gregory Nazianzen, Gregory Nyssa) hold a double-procession theology? Patristic reception history: when and where does ''and the Son'' first appear in creedal texts?',
    'If implicit, the magisterial definition clarifies rather than adds — lower extractiveness, stronger coordination claim. If novel, the definition innovates without ecumenical consent — higher extractiveness, snare-like unilateral imposition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(filioque_implicit_in_381, conceptual, 'Whether the Filioque is a clarification of or an addition to the 381 Creed.').

omega_variable(
    magisterial_authority_scope,
    'Does papal/conciliar magisterium possess authority to define implicit Trinitarian doctrine without ecumenical council reception by the East?',
    'Canonical analysis: Vatican I''s Pastor Aeternus vs. Eastern conciliar theology. Historical test: were Florence (1439) and Vatican I (1870) received by the Eastern churches? Theological test: does the development-of-doctrine principle (Newman) apply to Trinitarian dogma?',
    'If authority extends this far, the Filioque definition is legitimate coordination. If authority requires ecumenical reception, the definition is unilateral extraction. Determines whether the constraint is tangled_rope (coordination + extraction) or snare (pure extraction under coordination cover).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(magisterial_authority_scope, conceptual, 'Scope of magisterial authority to define doctrine unilaterally.').

omega_variable(
    suppression_mechanism_eastern_identity,
    'Is Eastern rejection of the Filioque maintained by structural barriers (canonical penalties, lack of communion) or internalized theological identity (monoprocession as constitutive of Orthodox self-understanding)?',
    'Post-exit trajectory analysis: if Eastern churches entered full communion with Rome while retaining mono-procession liturgy/theology (as some Eastern Catholic churches do), does suppression persist? Survey of Orthodox theologians: is Filioque rejection a matter of dogmatic conviction or ecclesial identity?',
    'If internalized, effective suppression is higher than structural measures suggest — the constraint travels with the agent after formal barriers drop. If structural, suppression would decrease with canonical recognition of mono-procession legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_eastern_identity, empirical, 'Structural vs. internalized suppression of Eastern theological autonomy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(creed_381_pneumatology__filioque_reading, 589, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(filioque_tr_t589, creed_381_pneumatology__filioque_reading, theater_ratio, 589, 0.15).
narrative_ontology:measurement(filioque_tr_t809, creed_381_pneumatology__filioque_reading, theater_ratio, 809, 0.25).
narrative_ontology:measurement(filioque_tr_t1014, creed_381_pneumatology__filioque_reading, theater_ratio, 1014, 0.35).
narrative_ontology:measurement(filioque_tr_t1054, creed_381_pneumatology__filioque_reading, theater_ratio, 1054, 0.4).
narrative_ontology:measurement(filioque_tr_t1274, creed_381_pneumatology__filioque_reading, theater_ratio, 1274, 0.42).
narrative_ontology:measurement(filioque_tr_t1439, creed_381_pneumatology__filioque_reading, theater_ratio, 1439, 0.45).
narrative_ontology:measurement(filioque_tr_t1870, creed_381_pneumatology__filioque_reading, theater_ratio, 1870, 0.48).
narrative_ontology:measurement(filioque_tr_t1965, creed_381_pneumatology__filioque_reading, theater_ratio, 1965, 0.42).
narrative_ontology:measurement(filioque_tr_t2024, creed_381_pneumatology__filioque_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(filioque_be_t589, creed_381_pneumatology__filioque_reading, base_extractiveness, 589, 0.25).
narrative_ontology:measurement(filioque_be_t809, creed_381_pneumatology__filioque_reading, base_extractiveness, 809, 0.35).
narrative_ontology:measurement(filioque_be_t1014, creed_381_pneumatology__filioque_reading, base_extractiveness, 1014, 0.55).
narrative_ontology:measurement(filioque_be_t1054, creed_381_pneumatology__filioque_reading, base_extractiveness, 1054, 0.68).
narrative_ontology:measurement(filioque_be_t1274, creed_381_pneumatology__filioque_reading, base_extractiveness, 1274, 0.72).
narrative_ontology:measurement(filioque_be_t1439, creed_381_pneumatology__filioque_reading, base_extractiveness, 1439, 0.78).
narrative_ontology:measurement(filioque_be_t1870, creed_381_pneumatology__filioque_reading, base_extractiveness, 1870, 0.78).
narrative_ontology:measurement(filioque_be_t1965, creed_381_pneumatology__filioque_reading, base_extractiveness, 1965, 0.75).
narrative_ontology:measurement(filioque_be_t2024, creed_381_pneumatology__filioque_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(filioque_su_t589, creed_381_pneumatology__filioque_reading, suppression_requirement, 589, 0.2).
narrative_ontology:measurement(filioque_su_t809, creed_381_pneumatology__filioque_reading, suppression_requirement, 809, 0.4).
narrative_ontology:measurement(filioque_su_t1014, creed_381_pneumatology__filioque_reading, suppression_requirement, 1014, 0.6).
narrative_ontology:measurement(filioque_su_t1054, creed_381_pneumatology__filioque_reading, suppression_requirement, 1054, 0.75).
narrative_ontology:measurement(filioque_su_t1274, creed_381_pneumatology__filioque_reading, suppression_requirement, 1274, 0.78).
narrative_ontology:measurement(filioque_su_t1439, creed_381_pneumatology__filioque_reading, suppression_requirement, 1439, 0.82).
narrative_ontology:measurement(filioque_su_t1870, creed_381_pneumatology__filioque_reading, suppression_requirement, 1870, 0.85).
narrative_ontology:measurement(filioque_su_t1965, creed_381_pneumatology__filioque_reading, suppression_requirement, 1965, 0.7).
narrative_ontology:measurement(filioque_su_t2024, creed_381_pneumatology__filioque_reading, suppression_requirement, 2024, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(creed_381_pneumatology__filioque_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(creed_381_pneumatology__filioque_reading, 0.08).
narrative_ontology:affects_constraint(creed_381_pneumatology__filioque_reading, creed_381_pneumatology__monoprocession_reading).
narrative_ontology:affects_constraint(creed_381_pneumatology__filioque_reading, creed_381_pneumatology__ecumenical_reunion_reading).

% DUAL FORMULATION NOTE:
% Constraint family: creed_381_pneumatology kernel decomposed into three readings. This reading (filioque) asserts magisterial definition authority; monoprocession asserts concilial inviolability; ecumenical_reunion asserts bilateral recognition. Each has distinct ε: filioque (0.78, high extraction via unilateral definition), monoprocession (0.15, low extraction but high resistance to imposition), ecumenical_reunion (0.35, moderate extraction via negotiated unity). Linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(creed_381_pneumatology__filioque_reading, institutional, 0.1).
constraint_indexing:directionality_override(creed_381_pneumatology__filioque_reading, organized, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
