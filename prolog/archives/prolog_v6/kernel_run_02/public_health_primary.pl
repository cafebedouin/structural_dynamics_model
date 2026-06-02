% ============================================================================
% CONSTRAINT STORY: public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_public_health_primary, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: public_health_primary
 *   human_readable: State Authority to Compel Vaccination for Public Health Protection
 *   domain: public_health_ethics/constitutional_law/medical_autonomy
 *
 * SUMMARY:
 *   This constraint embodies one reading of a fundamental contestation in
 *   public health ethics and constitutional law: when is state authority to
 *   compel medical intervention legitimate? The PUBLIC_HEALTH_PRIMARY reading
 *   anchors legitimacy in protecting vulnerable populations from serious
 *   harm. This is ONE reading among structurally distinct alternatives. Under
 *   this reading, state authority to mandate vaccination becomes justified
 *   when vulnerable populations (immunocompromised, infants unable to
 *   vaccinate, elderly with high mortality risk) face catastrophic harm from
 *   preventable disease. The mandate is a tangled rope: it provides genuine
 *   coordination function (collective immunity protects those who cannot be
 *   vaccinated) while simultaneously extracting costs from those who resist
 *   vaccination (employment loss, social exclusion, legal penalties). The
 *   extractiveness measurement (0.52) reflects that the coordination function
 *   is real but asymmetric enforcement creates extraction. The temporal
 *   trajectory shows extractiveness rising from 0.28 (initial phase, high
 *   coordination value, low enforcement cost) to 0.52 (mature phase,
 *   enforcement mechanisms mature, compliance becomes compulsory), indicating
 *   that as mandates persist beyond emergency conditions, their character
 *   shifts from coordination toward persistent extraction. Theater ratio
 *   (0.38) is moderate-low because the constraint has genuine functional
 *   content: vaccination actually does protect vulnerable populations. This
 *   distinguishes it from pure theater.
 *
 * KEY AGENTS:
 *   - Vulnerable populations (immunocompromised, infants, elderly): Powerless/trapped beneficiaries. Cannot vaccinate themselves or avoid disease through exit. Mandate protects them at cost of others' bodily autonomy.
 *   - Vaccine-hesitant individuals: Moderate/constrained victims. Face suppression (employment loss, exclusion, social penalty) but also receive coordination benefit (lower personal infection risk). Mixed experience.
 *   - Those with genuine medical contraindications: Moderate/trapped pure victims. Face mandate enforcement without benefit (cannot vaccinate). Genuine harm from mandate structure designed for different population.
 *   - Public health authorities: Powerful/arbitrage beneficiaries. Derive legitimacy from mandate implementation and disease reduction outcomes. Authority is conditional on demonstrating that vulnerable populations are actually protected.
 *   - International public health consensus (WHO, epidemiological institutions): Organized/mobile observers. See mandate legitimacy as contingent on disease severity and vulnerable population protection. Builders of sunset mechanisms and exit strategies.
 *   - Analytical observer at civilizational scope: Risk of naturalizing contingent political decision as immutable epidemiological law. False summit candidate.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_health_primary, 0.52).
domain_priors:suppression_score(public_health_primary, 0.68).
domain_priors:theater_ratio(public_health_primary, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_health_primary, extractiveness, 0.52).
narrative_ontology:constraint_metric(public_health_primary, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(public_health_primary, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_primary, tangled_rope).
narrative_ontology:human_readable(public_health_primary, "State Authority to Compel Vaccination for Public Health Protection").
narrative_ontology:topic_domain(public_health_primary, "public_health_ethics/constitutional_law/medical_autonomy").

domain_priors:requires_active_enforcement(public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_health_primary, '2912feb9-a3c1-4661-874e-7f5e0c4c35f7').
narrative_ontology:cs_created_at('2912feb9-a3c1-4661-874e-7f5e0c4c35f7', '').
narrative_ontology:cs_kernel_codification('2912feb9-a3c1-4661-874e-7f5e0c4c35f7', formalized).
narrative_ontology:cs_authority_grounding('2912feb9-a3c1-4661-874e-7f5e0c4c35f7', lineage).
narrative_ontology:cs_interpretation_layer_present('2912feb9-a3c1-4661-874e-7f5e0c4c35f7').
narrative_ontology:cs_kernel_id(public_health_primary, mandate_legitimacy_scope).
narrative_ontology:cs_reading_relation('2912feb9-a3c1-4661-874e-7f5e0c4c35f7', bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('2912feb9-a3c1-4661-874e-7f5e0c4c35f7', proportionality_reading, influences).
narrative_ontology:cs_axiom('2912feb9-a3c1-4661-874e-7f5e0c4c35f7', foundational, vulnerable_protection_paramount).
narrative_ontology:cs_axiom_status(vulnerable_protection_paramount, holdable).
narrative_ontology:cs_axiom_grounding('2912feb9-a3c1-4661-874e-7f5e0c4c35f7', vulnerable_protection_paramount, deontological).
narrative_ontology:cs_axiom('2912feb9-a3c1-4661-874e-7f5e0c4c35f7', secondary, collective_immunity_necessity).
narrative_ontology:cs_axiom_status(collective_immunity_necessity, holdable).
narrative_ontology:cs_axiom_grounding('2912feb9-a3c1-4661-874e-7f5e0c4c35f7', collective_immunity_necessity, empirically_contingent).
narrative_ontology:cs_reference_frame('2912feb9-a3c1-4661-874e-7f5e0c4c35f7', emergent_disease_protection_authority).
narrative_ontology:cs_drift_state('2912feb9-a3c1-4661-874e-7f5e0c4c35f7', endemic_disease_phase, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_health_primary, immunocompromised_populations).
narrative_ontology:constraint_beneficiary(public_health_primary, infants_ineligible_for_vaccination).
narrative_ontology:constraint_beneficiary(public_health_primary, collective_herd_immunity_threshold).
narrative_ontology:constraint_victim(public_health_primary, vaccine_hesitant_individuals).
narrative_ontology:constraint_victim(public_health_primary, those_with_medical_contraindications).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IMMUNOCOMPROMISED DEPENDENT (ROPE) — In this reading, the vulnerable population experiences the mandate as genuine coordination: it solves their collective action problem of exposure to preventable disease. They cannot vaccinate themselves or flee to low-disease zones. The mandate benefits them structurally. Experienced as protection rather than extraction, despite powerlessness.
constraint_indexing:constraint_classification(public_health_primary, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: VACCINE-HESITANT INDIVIDUAL (TANGLED ROPE) — Faces mandate enforcement (high suppression: loss of employment, exclusion from public spaces, social stigma) but mandate also provides genuine coordination function (reduces disease circulation, lowers personal infection risk). Mixed experience: extraction via compliance coercion AND coordination benefit. High suppression (0.68) reflects enforcement mechanisms; moderate extractiveness (0.52) reflects that some coordination value exists even under coercion.
constraint_indexing:constraint_classification(public_health_primary, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PUBLIC HEALTH AUTHORITY (TANGLED ROPE) — Possesses enforcement capacity and derives legitimacy from mandate implementation, but mandate's legitimacy depends on genuine disease reduction and vulnerable population protection. If vulnerable populations remain unprotected despite mandates, authority's claimed function (protection) fails and pure extraction is revealed. Power is conditional on demonstrated coordination outcome.
constraint_indexing:constraint_classification(public_health_primary, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INTERNATIONAL PUBLIC HEALTH CONSENSUS (SCAFFOLD) — Sees mandate legitimacy as contingent: required during high disease burden, declining as herd immunity approaches and vulnerable populations are protected. Sunset clause is endogenous: mandate legitimacy erodes when the justifying emergency condition resolves. Organized agents (WHO, national health authorities) are building exit strategies (surveillance systems, variant monitoring) that will trigger mandate relaxation. This reading treats mandate legitimacy as temporary coordination with built-in sunsetting logic.
constraint_indexing:constraint_classification(public_health_primary, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: MEDICAL CONTRAINDICATION CASE (SNARE) — Small population with genuine medical reasons for non-vaccination faces mandate enforcement without benefit. Suppression is total (exclusion from employment, public participation) with zero coordination gain (they cannot vaccinate regardless of mandate). Pure extraction with no escape: trapped, bearing full cost of mandate structure designed for different population.
constraint_indexing:constraint_classification(public_health_primary, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational scope, the constraint appears as an immutable property of epidemiology: above a certain vaccination threshold (R < 1), disease transmission stops regardless of mandate enforcement. The mandate appears as merely discovering and instantiating a natural boundary condition. However, this reading naturalizes what is actually a political decision about who bears the cost of reaching that threshold. The reading is a false-summit candidate: beneficiary presence (immunocompromised populations) and competing readings reveal that threshold legitimacy is contingent, not inevitable.
constraint_indexing:constraint_classification(public_health_primary, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(public_health_primary_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(public_health_primary, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(public_health_primary, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The mandate provides genuine coordination function — vaccination protects those who cannot vaccinate and reduces disease circulation. But enforcement creates extraction: individuals who oppose vaccination bear costs (employment loss, exclusion from public spaces, fines, criminal penalties) regardless of their personal risk profile or medical contraindications. The value 0.52 reflects that the coordination function is real (~0.40 inherent to any disease control mechanism) but enforcement asymmetry adds extraction (~0.12 from coercion beyond the coordination minimum). Suppression (0.68): Moderate-high. Barriers to non-vaccination include: employment loss in healthcare/public sectors, exclusion from public facilities, loss of professional licenses, travel restrictions, loss of educational access, social stigma. However, suppression is not total — some jurisdictions allow exemptions (medical, religious, philosophical) and some individuals can absorb costs. Theater ratio (0.38): Moderate-low. The constraint has functional content: vaccination genuinely does reduce transmission and protect vulnerable populations. However, some performative elements exist: public health messaging may overstate certainty about vaccine efficacy, mandate implementation may prioritize visible enforcement over actual vulnerable population protection, and political commitment to mandates may persist past the point of genuine public health necessity.
 *
 * PERSPECTIVAL GAP:
 *   The perspectives range from Rope (vulnerable populations experiencing protection) through Tangled Rope (hesitant individuals experiencing mixed coordination and coercion) and Snare (medical contraindication cases experiencing pure harm) to Scaffold (organized international agents seeing temporary coordination) and Mountain (analytical observer risking naturalization). The vulnerable populations see the mandate as solving their coordination problem — they want protection and cannot achieve it independently. The hesitant individuals see mandate as imposed coercion with some incidental benefit (disease reduction they'd prefer anyway). The contraindication cases see pure harm — coercion without benefit. The international public health consensus sees sunsetting logic — mandate legitimacy decays as disease severity decays. The analytical observer risks seeing an immutable law — 'collective immunity requires mandate at threshold T' — but this naturalizes what is actually a decision about whose costs matter. The perspectival gap reveals whether the constraint is genuinely coordination (benefits everyone at some cost) or extraction (benefits some while extracting from others). In this case: genuinely mixed.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position relative to the constraint. Vulnerable populations (immunocompromised, infants) are beneficiaries with zero exit options (trapped) — they cannot vaccinate themselves and cannot move to lower-disease zones. Their derived d is low (~0.15), yielding negative or near-zero effective extraction f(d) ≈ -0.01 to 0.02. They experience the mandate as coordination, not extraction. Vaccine-hesitant individuals are victims with constrained exit (employment cost, social cost, but not physical confinement) — they can exit by relocating or absorbing penalties. Their d is ~0.60, yielding f(d) ≈ 0.65, moderate experienced extraction. Those with medical contraindications are victims with trapped exit (they cannot vaccinate regardless of mandate) — their d is ~0.95, yielding f(d) ≈ 1.42, maximum experienced extraction despite being unable to contribute to disease transmission. Public health authorities are beneficiaries with arbitrage exit (can implement or withdraw mandates; gain authority from implementation) — their d is ~0.05, yielding f(d) ≈ -0.12. The chi formula χ = ε × f(d) × σ(S) with σ(national) = 1.0 produces: beneficiaries experience χ ≈ 0.52 × (-0.01) × 1.0 ≈ -0.005 (small negative); hesitant experience χ ≈ 0.52 × 0.65 × 1.0 ≈ 0.34; contraindicated experience χ ≈ 0.52 × 1.42 × 1.0 ≈ 0.74. The divergence in experienced extractiveness across agent types is the diagnostic signal that this is a tangled rope: genuine coordination function coexists with asymmetric enforcement.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy by explicitly modeling the asymmetric coordination function. Traditional utilitarian framing would collapse toward Rope ('aggregate health benefit justifies mandate') or Snare ('individual autonomy is violated'). This reading holds both simultaneously: the mandate provides genuine coordination (vulnerable population protection) AND extracts costs (compliance coercion). The tangled rope classification is stable because: (1) beneficiaries are identifiable and real (immunocompromised populations have measurable vulnerability), (2) victims are identifiable and real (hesitant individuals face measurable suppression), (3) enforcement is active (mandate requires administrative infrastructure), (4) coordination function exists (vaccination does protect vulnerable populations). The ambiguity that remains is whether the coordination function justifies the extraction — this is resolved by omega variables (vulnerable population definition, alternative protection mechanisms, sunset conditions), not by classification drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vulnerable_population_definition,
    'Who counts as ''vulnerable'' for purposes of mandate legitimacy? Are immunocompromised, infants, and elderly the only victims the mandate protects, or does the definition expand to include economically vulnerable, geographically isolated, or institutionalized populations?',
    'Population-level epidemiological data: differential infection rates, hospitalization rates, and mortality by vulnerability status before and after mandate implementation. Policy document analysis of how vulnerable population is operationalized in mandate enforcement rules.',
    'If definition is narrow (medical vulnerability only): extractiveness remains ~0.52, beneficiary/victim balance is defensible. If definition expands: mandate becomes cover story for broader social control, extractiveness rises to 0.65+, classification shifts toward Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vulnerable_population_definition, empirical, 'Operational definition of vulnerable population in mandate legitimacy').

omega_variable(
    alternative_protection_mechanisms,
    'Could targeted protection (shielding strategies for vulnerable populations, rapid treatment access for high-risk groups) achieve equivalent health outcomes as universal mandate without compulsion?',
    'Comparative epidemiological modeling of mandate vs. targeted protection scenarios; historical case studies of disease control with and without mandates; cost-effectiveness analysis of protection strategies.',
    'If alternatives are equivalently effective: mandate appears as extraction mechanism disguised as necessary public health measure. Extractiveness rises, classification becomes Snare. If alternatives are substantially less effective: mandate''s coordination function is confirmed and extractiveness remains justified at current level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_protection_mechanisms, empirical, 'Efficacy of alternative protection mechanisms vs. mandate-based approach').

omega_variable(
    mandate_sunset_trigger,
    'Under what observable condition should mandate legitimacy end? Is the trigger herd immunity threshold, elimination of the pathogen, time-bound expiration, or something else?',
    'Policy analysis: do mandate-implementing authorities declare explicit sunset conditions? Historical analysis: have mandates been lifted when triggering conditions were met, or do they persist indefinitely? Epidemiological assessment of whether herd immunity threshold has been reached.',
    'If explicit sunset exists and is honored: scaffold reading is correct, extractiveness interpretation is valid. If sunset is vague or non-existent: mandate appears as permanent power grab, extractiveness becomes source of ongoing harm, classification shifts toward persistent Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandate_sunset_trigger, empirical, 'Observable conditions for mandate legitimacy termination').

omega_variable(
    reading_framework_collision,
    'This is one reading of the mandate_legitimacy_scope kernel. The bodily_autonomy_primary reading treats individual bodily integrity as the foundational legitimacy anchor. Can these readings coexist in a single legal/ethical framework, or does commitment to one preclude the other?',
    'Jurisprudential analysis: can a court, legislature, or institutional authority simultaneously hold both that (a) state protection of vulnerable populations can override individual mandate resistance AND (b) individual bodily autonomy is inviolable except in extreme circumstances? Historical analysis of how actual institutions navigate the collision.',
    'If readings foreclose each other: one must be chosen, classification of sibling constraint changes. If readings coexist: both constraints are valid framings of the same phenomenon held by different institutional actors. If readings influence without foreclosing: public_health_primary constrains bodily_autonomy_primary''s domain but doesn''t eliminate it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_framework_collision, conceptual, 'Mutual foreclosure or coexistence of public_health_primary and bodily_autonomy_primary readings').

omega_variable(
    proportionality_measurement,
    'Under what conditions is mandate extractiveness (0.52) ''proportional'' to the public health harm being prevented? Is proportionality inherent to this reading or contingent on empirical disease severity?',
    'Proportionality doctrine analysis: case law on narrow tailoring, least restrictive means, compelling state interest. Empirical assessment: disease mortality rates, hospitalization burden, vulnerable population size during mandate implementation. Compare extractiveness magnitude to health harm magnitude.',
    'If proportionality is inherent: extractiveness is justified regardless of disease severity (reading claims universal applicability). If proportionality is contingent: extractiveness value depends on observable disease burden, and mandate legitimacy degrades as disease severity drops.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_measurement, conceptual, 'Proportionality of mandate extractiveness to public health harm prevented').

omega_variable(
    reading_codification_status,
    'Is public_health_primary an established reading of the mandate legitimacy kernel, or is it an emergent/contested reading in contemporary discourse? Has it been formally adjudicated (constitutional court, statute) or does it remain a normative claim under debate?',
    'Legal history: search for precedent cases establishing public health authority to mandate medical interventions (Jacobson v. Massachusetts, state emergency powers cases). Policy document analysis: how do health agencies frame mandate legitimacy? Jurisprudential analysis: has the reading been formalized in constitutional doctrine or does it remain implicit in administrative practice?',
    'If formally established: reading has institutional authority and can ground legitimacy claims. If emergent: reading''s authority is weaker and more vulnerable to challenge from bodily_autonomy_primary reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_codification_status, empirical, 'Legal-institutional codification status of public_health_primary reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_primary, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(publ_tr_t0, public_health_primary, theater_ratio, 0, 0.25).
narrative_ontology:measurement(publ_tr_t6, public_health_primary, theater_ratio, 6, 0.32).
narrative_ontology:measurement(publ_tr_t12, public_health_primary, theater_ratio, 12, 0.38).
narrative_ontology:measurement(publ_tr_t18, public_health_primary, theater_ratio, 18, 0.41).

% Extraction over time
narrative_ontology:measurement(publ_be_t0, public_health_primary, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(publ_be_t6, public_health_primary, base_extractiveness, 6, 0.42).
narrative_ontology:measurement(publ_be_t12, public_health_primary, base_extractiveness, 12, 0.52).
narrative_ontology:measurement(publ_be_t18, public_health_primary, base_extractiveness, 18, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_health_primary, resource_allocation).
narrative_ontology:affects_constraint(public_health_primary, bodily_autonomy_primary).
narrative_ontology:affects_constraint(public_health_primary, proportionality_reading).
narrative_ontology:affects_constraint(public_health_primary, medical_exception_burden).

% DUAL FORMULATION NOTE:
% The mandate_legitimacy_scope kernel decomposes into three constraint stories representing distinct readings: public_health_primary (this file) anchors legitimacy in vulnerable population protection; bodily_autonomy_primary anchors legitimacy in individual bodily integrity; proportionality_reading anchors legitimacy in narrow tailoring. Each has different ε values and different perspectives. public_health_primary affects bodily_autonomy_primary by creating structural pressure toward narrower exceptions; bodily_autonomy_primary affects public_health_primary by creating pressure to demonstrate genuine vulnerable population benefit; proportionality_reading constrains both by requiring justification calibration. Network edges flow downstream: public_health_primary → bodily_autonomy_primary (challenge to autonomy primacy); public_health_primary → medical_exception_burden (defines whose medical contraindications count).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(public_health_primary, powerful, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
