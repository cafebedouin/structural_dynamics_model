% ============================================================================
% CONSTRAINT STORY: creed_381_pneumatology__filioque_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Filioque Reading: Papal Authority to Clarify Trinitarian Procession
 *   domain: historical_theology/ecclesiastical_authority
 *
 * SUMMARY:
 *   This story authors the Filioque reading of the contested 381 pneumatology
 *   kernel: the Spirit proceeds 'from the Father and the Son,' and the
 *   papal/conciliar magisterium possesses authority to make this
 *   clarification explicit even without renewed ecumenical consent. From
 *   roughly the regional Spanish/Visigothic adoption (589, Third Council of
 *   Toledo) through Carolingian promotion (794, 809), Roman liturgical
 *   adoption (1014), the mutual excommunications of 1054, the failed reunion
 *   at Lyon (1274), and the failed reunion at Florence (1439), this reading
 *   was progressively formalized, defended, and eventually treated by the
 *   Latin Church as settled doctrine binding on the whole Church. The
 *   extraction this story authors is not financial but jurisdictional and
 *   theological: unilateral Roman/Western definitional authority displacing
 *   conciliar consensus and Eastern theological autonomy, with permanent
 *   schism as the structural cost borne by the Christian East.
 *
 * KEY AGENTS:
 *   - roman_see: agenda-setter and primary beneficiary, exercises unilateral clarificatory authority
 *   - carolingian_frankish_church: early adopter/promoter, political-theological beneficiary
 *   - eastern_patriarchates: primary payer, bears loss of conciliar veto and communion rupture
 *   - byzantine_theological_tradition: payer, entire theological grammar delegitimized
 *   - lay_believers_east_and_west: excluded, inherit permanent schism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(creed_381_pneumatology__filioque_reading, 0.72).
domain_priors:suppression_score(creed_381_pneumatology__filioque_reading, 0.68).
domain_priors:theater_ratio(creed_381_pneumatology__filioque_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(creed_381_pneumatology__filioque_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(creed_381_pneumatology__filioque_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(creed_381_pneumatology__filioque_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(creed_381_pneumatology__filioque_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(creed_381_pneumatology__filioque_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(creed_381_pneumatology__filioque_reading, tangled_rope).
narrative_ontology:human_readable(creed_381_pneumatology__filioque_reading, "Filioque Reading: Papal Authority to Clarify Trinitarian Procession").
narrative_ontology:topic_domain(creed_381_pneumatology__filioque_reading, "historical_theology/ecclesiastical_authority").

domain_priors:requires_active_enforcement(creed_381_pneumatology__filioque_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(creed_381_pneumatology__filioque_reading, 'a603ab2c-63b8-43a3-acf9-f323e73799a2').
narrative_ontology:cs_kernel_codification('a603ab2c-63b8-43a3-acf9-f323e73799a2', fixed_text).
narrative_ontology:cs_authority_grounding('a603ab2c-63b8-43a3-acf9-f323e73799a2', lineage).
narrative_ontology:cs_interpretation_layer_present('a603ab2c-63b8-43a3-acf9-f323e73799a2').
narrative_ontology:cs_reading_relation('a603ab2c-63b8-43a3-acf9-f323e73799a2', creed_381_pneumatology__monoprocession_reading, forecloses).
narrative_ontology:cs_reading_relation('a603ab2c-63b8-43a3-acf9-f323e73799a2', creed_381_pneumatology__ecumenical_reunion_reading, influences).
narrative_ontology:cs_axiom('a603ab2c-63b8-43a3-acf9-f323e73799a2', foundational, papal_magisterium_may_clarify_implicit_doctrine_unilaterally).
narrative_ontology:cs_axiom_status(papal_magisterium_may_clarify_implicit_doctrine_unilaterally, holdable).
narrative_ontology:cs_axiom_grounding('a603ab2c-63b8-43a3-acf9-f323e73799a2', papal_magisterium_may_clarify_implicit_doctrine_unilaterally, conventional).
narrative_ontology:cs_axiom('a603ab2c-63b8-43a3-acf9-f323e73799a2', foundational, double_procession_necessary_for_full_trinitarian_consubstantiality).
narrative_ontology:cs_axiom_status(double_procession_necessary_for_full_trinitarian_consubstantiality, holdable).
narrative_ontology:cs_axiom_grounding('a603ab2c-63b8-43a3-acf9-f323e73799a2', double_procession_necessary_for_full_trinitarian_consubstantiality, deontological).
narrative_ontology:cs_axiom('a603ab2c-63b8-43a3-acf9-f323e73799a2', secondary, regional_synodal_adoption_can_precede_and_ground_universal_binding_force).
narrative_ontology:cs_axiom_status(regional_synodal_adoption_can_precede_and_ground_universal_binding_force, holdable).
narrative_ontology:cs_axiom_grounding('a603ab2c-63b8-43a3-acf9-f323e73799a2', regional_synodal_adoption_can_precede_and_ground_universal_binding_force, conventional).
narrative_ontology:cs_reference_frame('a603ab2c-63b8-43a3-acf9-f323e73799a2', petrine_universal_magisterium).
narrative_ontology:cs_drift_state('a603ab2c-63b8-43a3-acf9-f323e73799a2', post_1054_schism, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('a603ab2c-63b8-43a3-acf9-f323e73799a2', '').
narrative_ontology:cs_kernel_id(creed_381_pneumatology__filioque_reading, creed_381_pneumatology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(creed_381_pneumatology__filioque_reading, roman_see).
narrative_ontology:constraint_beneficiary(creed_381_pneumatology__filioque_reading, latin_theological_tradition).
narrative_ontology:constraint_beneficiary(creed_381_pneumatology__filioque_reading, carolingian_frankish_church).
narrative_ontology:constraint_victim(creed_381_pneumatology__filioque_reading, eastern_patriarchates).
narrative_ontology:constraint_victim(creed_381_pneumatology__filioque_reading, byzantine_theological_tradition).
narrative_ontology:constraint_victim(creed_381_pneumatology__filioque_reading, local_conciliar_autonomy).
narrative_ontology:constraint_vindicates(creed_381_pneumatology__filioque_reading, papal_universal_magisterial_authority).
narrative_ontology:constraint_vindicates(creed_381_pneumatology__filioque_reading, doctrinal_development_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authorizes and eventually formally adopts the Filioque clause into the Latin liturgical creed, asserting authority to clarify doctrine implicit in scripture and tradition even absent a new ecumenical council. Consolidates its claim to universal jurisdiction over doctrinal definition; the clause becomes a marker of communion with Rome.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, roman_see, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(creed_381_pneumatology__filioque_reading, roman_see, beneficiary).

% Adopted and promoted the Filioque locally well before Rome's formal adoption, using it to assert theological and political distinctiveness from Byzantium and to bolster Carolingian imperial legitimacy against the Eastern Empire. Gains status as doctrinal innovator later ratified by Rome.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, carolingian_frankish_church, beneficiary,
    powerful, generational, mobile, continental).

% Regard the unilateral addition to the 381 creed text as a violation of conciliar consensus and an assertion of Roman jurisdiction over doctrine that was never granted. Their theological objection (that the clause misconstrues the Father as sole source, arche, of the Trinity) is treated by this reading as answered rather than live. Their exit options are constrained to schism, which carries civilizational cost.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, eastern_patriarchates, payer,
    powerful, civilizational, constrained, continental).

% An entire theological grammar built on monopatrism (single-source procession safeguarding the Father's monarchy) is rendered heterodox or at best a lesser regional variant by this reading's framework. Cannot simply exit the disagreement without abandoning core theological commitments that define its tradition.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, byzantine_theological_tradition, payer,
    organized, civilizational, trapped, continental).

% The principle that creedal text is fixed except by ecumenical council is displaced by unilateral regional/papal amendment; this is not an actor but the institutional practice that bears the structural cost of the reading's success, listed for completeness.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, local_conciliar_autonomy, payer,
    institutional, civilizational, trapped, continental).
narrative_ontology:stakeholder_non_agent(creed_381_pneumatology__filioque_reading, local_conciliar_autonomy).

% Scholastic theology (Augustinian psychological analogy of the Trinity) finds doctrinal confirmation and further elaboration ground in the Filioque; Western theological schools gain a settled premise from which to build subsequent Trinitarian and pneumatological reflection.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, latin_theological_tradition, beneficiary,
    organized, civilizational, mobile, continental).

% Ordinary believers on both sides inherit a permanent schism and mutual anathemas over a clause whose technical Trinitarian content most never articulated; their liturgical and communal life is reorganized by a dispute conducted entirely among clerical and imperial elites.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, lay_believers_east_and_west, excluded,
    powerless, generational, trapped, local).

% The mechanism that produced the original 381 text (and that the East insists is the only legitimate amending body) is bypassed by this reading's method of doctrinal clarification; the process itself has no voice in whether it may be circumvented.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, ecumenical_council_process, excluded,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(creed_381_pneumatology__filioque_reading, ecumenical_council_process).

% Modern theologians and dialogue commissions reassess whether the Filioque dispute reflects genuine doctrinal contradiction or translatable idiom (per Filioque vs. dia tou Huiou), and whether papal clarificatory authority as exercised in this reading was validly constituted.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, later_ecumenists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, authoritatively settled formula for the Spirit's procession, permitting the Latin Church to move forward theologically and liturgically without waiting on cross-communion consensus, and allowing Rome to exercise a general magisterial function of clarifying doctrine implicit but not explicit in earlier conciliar text.
% TRANSFER_FUNCTION: Moves doctrinal defining authority from the ecumenical council (a body requiring Eastern participation and consent) to the Roman see and Western regional synods acting unilaterally; moves theological legitimacy away from the Byzantine monopatrist framework and toward the Augustinian/Western framework; moves practical communion status away from churches that will not adopt the clause.
% ABSENT_VOICES: The Eastern patriarchates were not consulted on, and explicitly rejected, the insertion at the moment of Frankish and later Roman adoption; no ecumenical council with full Eastern participation ratified the change. Their objection is treated in Western sources of this reading as a matter to be overcome by explanation rather than as a veto requiring their consent.
% DISAPPEARANCE_RATIONALE: If papal/conciliar authority to unilaterally clarify the creed's pneumatology were withdrawn, the Filioque clause would revert to contested/local status, communion with the East would become theologically easier to restore, and the entire subsequent Western doctrine of expansive magisterial authority to develop doctrine without ecumenical consent would lose its founding precedent — centuries of ecclesiology built on this precedent would need re-grounding.
% FOUNDING_PROBLEM: Western theologians (originally in Visigothic Spain against Arianism, later Carolingian Francia) sought a formula emphasizing the full divinity and consubstantiality of the Son by asserting the Spirit's procession from both Father and Son, and Rome sought to consolidate a doctrinal and jurisdictional claim that it could authoritatively state what earlier councils left implicit.
% FOUNDING_PROBLEM_CORROBORATION: Western canonists and successive popes attest the clarification was doctrinally necessary and validly exercised. Eastern patriarchs, Orthodox theologians, and modern ecumenical dialogue commissions (including some Catholic-Orthodox joint statements, e.g. the 1995 Vatican clarification acknowledging the Eastern formula as equally valid) attest from outside the Latin beneficiary tradition that the unilateral insertion was uncanonical and that the underlying theological problem may have been better resolved through differentiated but jointly-recognized expression rather than imposed uniformity.
narrative_ontology:disappearance_verdict(creed_381_pneumatology__filioque_reading, world_rearranges).
narrative_ontology:founding_problem_status(creed_381_pneumatology__filioque_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(creed_381_pneumatology__filioque_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(creed_381_pneumatology__filioque_reading, 'none', 1).
narrative_ontology:epsilon_provenance(creed_381_pneumatology__filioque_reading, 0.72, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored high (0.72 by 1439) because the reading's core mechanism is a jurisdictional transfer: definitional authority over Trinitarian doctrine moves from a body requiring Eastern consent (ecumenical council) to a body that does not require it (Rome/regional synod), and this transfer is treated within the reading as valid and binding on those who did not consent. Suppression is authored high (0.68) because maintaining the clause as universally binding requires ongoing assertion against a large, organized, theologically serious dissenting tradition (mutual anathemas, refusal of intercommunion) rather than passive acceptance. Theater ratio rises over the interval (0.2 to 0.4+) as failed reunion councils (Lyon 1274, Florence 1439) produced formal agreements that did not translate into lived reconciliation — increasing performative resolution without functional resolution.
 *
 * PERSPECTIVAL GAP:
 *   From the Roman see's seat this is a rope: a genuine coordination function (doctrinal clarity, unified Trinitarian confession) exercised through legitimate, gradually-developed magisterial authority. From the Eastern patriarchates' seat the identical structure is a tangled rope shading toward snare: coordination language covers a jurisdictional seizure that overrides their conciliar veto and reclassifies their inherited orthodoxy as deficient. The engine should compute these divergently from the same structural data — the claimed_type of tangled_rope is authored from the analytical seat that takes both the real coordination function (a settled Trinitarian formula does serve some doctrinal-clarity purpose) and the asymmetric extraction (unilateral imposition, Eastern autonomy overridden) as both genuinely present.
 *
 * DIRECTIONALITY LOGIC:
 *   Roman see and Carolingian/Latin theological tradition are coded as beneficiaries: they gain jurisdictional consolidation, theological completion of their preferred Augustinian framework, and (for the Franks) political legitimacy vis-a-vis Byzantium — low d, benefits flow toward them. Eastern patriarchates and Byzantine theological tradition are coded as victims: they bear the cost of schism, the delegitimization of monopatrism, and loss of the conciliar veto they consider constitutionally necessary for creedal change — high d, trapped/constrained exit reflecting that leaving the dispute means either capitulation or permanent rupture. Lay believers are excluded rather than victim-coded directly, since the extraction operates at the institutional/doctrinal level and reaches them only derivatively through inherited schism.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (safeguarding full Trinitarian consubstantiality of the Son against residual Arianism) was substantively resolved by the time of Carolingian and Roman adoption in the West; by 1054 the clause functioned less as anti-Arian defense and more as a marker of Roman jurisdictional supremacy and Latin theological distinctiveness. Reading founding_problem_status as contested rather than dead reflects that Latin sources still treat the anti-Arian/Trinitarian-completeness rationale as live, while Eastern and later ecumenical sources treat the original problem as resolved and the persisting clause as serving a different, jurisdictional function — exactly the founding-problem/disappearance-verdict mismatch the framework is built to surface (status=contested, verdict=world_rearranges, which is not the dead/world_rearranges capture-flag pattern but sits adjacent to it and warrants the same scrutiny).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    development_vs_innovation,
    'Is the Filioque a legitimate development of doctrine implicit in earlier Trinitarian theology (per this reading''s own claim), or a substantive innovation that changes the content of the 381 creed without the consent the creed''s own amending process requires?',
    'Patristic-textual analysis of whether pre-381 and immediately post-381 Greek and Latin Fathers held a doctrine of double procession implicitly, combined with analysis of the canonical status of unilateral conciliar text amendment in the ecclesiology of the period.',
    'If genuinely implicit and merely clarified, the reading''s coordination framing (rope-like) gains support; if a substantive change imposed without required consent, the tangled_rope/snare-leaning extraction framing is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(development_vs_innovation, conceptual, 'Whether Filioque is doctrinal clarification or unilateral doctrinal innovation.').

omega_variable(
    papal_authority_scope_kernel,
    'Does papal/conciliar magisterial authority extend to unilateral clarification of ecumenically-defined creedal text, or is such authority itself only validly exercised through renewed ecumenical process?',
    'This is the deepest structural disagreement between the filioque_reading and monoprocession_reading and is not resolvable by evidence internal to either tradition; it depends on which ecclesiology of authority (Roman universal jurisdiction vs. conciliar/synodal consensus) is adopted as prior.',
    'Adopting the Roman jurisdictional premise validates this reading''s claimed_type leaning toward rope/tangled_rope from the beneficiary seat; adopting the conciliar premise collapses this reading toward snare from an analytical seat, since the ''coordination'' claim depends entirely on an authority structure the victims never granted.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(papal_authority_scope_kernel, conceptual, 'Whether papal unilateral doctrinal clarification is a valid exercise of magisterial authority.').

omega_variable(
    linguistic_equivalence_omega,
    'Does the Latin ''Filioque'' and the Greek ''dia tou Huiou'' (through the Son) express the same underlying theological content in different idiom, such that the dispute is substantially terminological rather than doctrinal?',
    'Comparative historical theology and the 1995 Vatican Pontifical Council for Promoting Christian Unity clarification, which itself treated the formulas as potentially reconcilable; weight given to modern joint Catholic-Orthodox theological commissions.',
    'If substantially terminological, this reading''s high authored extraction may overstate the actual doctrinal stakes relative to the jurisdictional stakes — suggesting the true extraction is almost entirely about authority, not content. If substantively different, the doctrinal dimension of the extraction is independently real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(linguistic_equivalence_omega, empirical, 'Whether the Filioque/dia tou Huiou dispute is terminological or substantively doctrinal.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(creed_381_pneumatology__filioque_reading, 589, 1439).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cree_tr_t589, creed_381_pneumatology__filioque_reading, theater_ratio, 589, 0.2).
narrative_ontology:measurement_basis(cree_tr_t589, observed).
narrative_ontology:measurement(cree_tr_t794, creed_381_pneumatology__filioque_reading, theater_ratio, 794, 0.25).
narrative_ontology:measurement_basis(cree_tr_t794, observed).
narrative_ontology:measurement(cree_tr_t809, creed_381_pneumatology__filioque_reading, theater_ratio, 809, 0.28).
narrative_ontology:measurement_basis(cree_tr_t809, observed).
narrative_ontology:measurement(cree_tr_t1014, creed_381_pneumatology__filioque_reading, theater_ratio, 1014, 0.32).
narrative_ontology:measurement_basis(cree_tr_t1014, observed).
narrative_ontology:measurement(cree_tr_t1054, creed_381_pneumatology__filioque_reading, theater_ratio, 1054, 0.38).
narrative_ontology:measurement_basis(cree_tr_t1054, observed).
narrative_ontology:measurement(cree_tr_t1274, creed_381_pneumatology__filioque_reading, theater_ratio, 1274, 0.45).
narrative_ontology:measurement_basis(cree_tr_t1274, observed).
narrative_ontology:measurement(cree_tr_t1439, creed_381_pneumatology__filioque_reading, theater_ratio, 1439, 0.4).
narrative_ontology:measurement_basis(cree_tr_t1439, observed).

% Extraction over time
narrative_ontology:measurement(cree_be_t589, creed_381_pneumatology__filioque_reading, base_extractiveness, 589, 0.35).
narrative_ontology:measurement_basis(cree_be_t589, observed).
narrative_ontology:measurement(cree_be_t794, creed_381_pneumatology__filioque_reading, base_extractiveness, 794, 0.48).
narrative_ontology:measurement_basis(cree_be_t794, observed).
narrative_ontology:measurement(cree_be_t809, creed_381_pneumatology__filioque_reading, base_extractiveness, 809, 0.55).
narrative_ontology:measurement_basis(cree_be_t809, observed).
narrative_ontology:measurement(cree_be_t1014, creed_381_pneumatology__filioque_reading, base_extractiveness, 1014, 0.65).
narrative_ontology:measurement_basis(cree_be_t1014, observed).
narrative_ontology:measurement(cree_be_t1054, creed_381_pneumatology__filioque_reading, base_extractiveness, 1054, 0.78).
narrative_ontology:measurement_basis(cree_be_t1054, observed).
narrative_ontology:measurement(cree_be_t1274, creed_381_pneumatology__filioque_reading, base_extractiveness, 1274, 0.7).
narrative_ontology:measurement_basis(cree_be_t1274, observed).
narrative_ontology:measurement(cree_be_t1439, creed_381_pneumatology__filioque_reading, base_extractiveness, 1439, 0.72).
narrative_ontology:measurement_basis(cree_be_t1439, observed).

% Suppression requirement over time
narrative_ontology:measurement(cree_su_t589, creed_381_pneumatology__filioque_reading, suppression_requirement, 589, 0.3).
narrative_ontology:measurement_basis(cree_su_t589, observed).
narrative_ontology:measurement(cree_su_t794, creed_381_pneumatology__filioque_reading, suppression_requirement, 794, 0.4).
narrative_ontology:measurement_basis(cree_su_t794, observed).
narrative_ontology:measurement(cree_su_t809, creed_381_pneumatology__filioque_reading, suppression_requirement, 809, 0.5).
narrative_ontology:measurement_basis(cree_su_t809, observed).
narrative_ontology:measurement(cree_su_t1014, creed_381_pneumatology__filioque_reading, suppression_requirement, 1014, 0.62).
narrative_ontology:measurement_basis(cree_su_t1014, observed).
narrative_ontology:measurement(cree_su_t1054, creed_381_pneumatology__filioque_reading, suppression_requirement, 1054, 0.75).
narrative_ontology:measurement_basis(cree_su_t1054, observed).
narrative_ontology:measurement(cree_su_t1274, creed_381_pneumatology__filioque_reading, suppression_requirement, 1274, 0.6).
narrative_ontology:measurement_basis(cree_su_t1274, observed).
narrative_ontology:measurement(cree_su_t1439, creed_381_pneumatology__filioque_reading, suppression_requirement, 1439, 0.68).
narrative_ontology:measurement_basis(cree_su_t1439, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(creed_381_pneumatology__filioque_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(creed_381_pneumatology__filioque_reading, 0.1).
narrative_ontology:affects_constraint(creed_381_pneumatology__filioque_reading, monoprocession_reading).
narrative_ontology:affects_constraint(creed_381_pneumatology__filioque_reading, ecumenical_reunion_reading).
narrative_ontology:affects_constraint(creed_381_pneumatology__filioque_reading, papal_primacy_universal_jurisdiction_claim).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the creed_381_pneumatology kernel. monoprocession_reading authors the Eastern position (381 text inviolable absent ecumenical consent) with victims and beneficiaries substantially inverted relative to this story. ecumenical_reunion_reading authors a later, lower-extraction reading in which bilateral recognition of both formulas replaces unilateral imposition, effectively dissolving the tangled_rope structure into something closer to a rope. All three share the same kernel text (381 creed) and the same underlying procession question but instantiate structurally distinct ε values, beneficiary/victim sets, and classifications per the ε-invariance principle — they are not the same constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
