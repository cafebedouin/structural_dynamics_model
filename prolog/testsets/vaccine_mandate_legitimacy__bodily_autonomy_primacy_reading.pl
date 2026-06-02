% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading
 *   human_readable: Vaccine Mandate Legitimacy (Bodily Autonomy Primacy Reading)
 *   domain: public_health_policy/constitutional_law/bioethics
 *
 * SUMMARY:
 *   The vaccine mandate legitimacy constraint is a contested kernel with
 *   three distinct readings, each instantiating a structurally different
 *   claim about state authority and bodily autonomy. This JSON generates the
 *   bodily autonomy primacy reading: the claim that medical self-sovereignty
 *   is absolute and state coercion categorically impermissible regardless of
 *   public health outcomes. Under this reading, the mandate is a snare — a
 *   pure extraction mechanism where the state eliminates alternatives and
 *   coerces compliance through monopolization of legitimate participation
 *   pathways (employment, education, public life). The structural data
 *   reveals this reading naturalizes a contingent political position: the
 *   beneficiaries are identifiable (public health authorities consolidating
 *   institutional power), the extraction mechanism is contingent on
 *   enforcement capacity, and the suppression is engineered through
 *   elimination of alternatives, not inherent to the constraint. The
 *   immunocompromised vulnerable population enters the victim set because
 *   they face asymmetric exposure risk if vaccination rates fall below
 *   protective levels — they bear the extraction burden from both coercive
 *   mandate (if exemptions are denied) and from unvaccinated populations (if
 *   exemptions are honored). This reading differs structurally from the
 *   public health primacy reading (which classifies the mandate as a tangled
 *   rope with genuine coordination function) and from the risk stratification
 *   reading (which classifies it as a scaffold with differentiated
 *   requirements based on vulnerability). All three readings contest the same
 *   kernel — the legitimate scope of state power over bodily integrity — but
 *   reach opposite conclusions about whether mandates are categorically
 *   impermissible, contextually justified, or optimally risk-stratified.
 *
 * KEY AGENTS:
 *   - Vaccine Refusers: Primary victims (powerless/trapped) — face coercive choice: comply or lose participation in employment, education, public life
 *   - Immunocompromised Vulnerable: Secondary victims (powerless/trapped) — bear asymmetric exposure risk; may face exemption denial or inadequate protection
 *   - Public Health Authority: Primary beneficiary (institutional/arbitrage) — consolidates institutional power, establishes behavioral standard, leverages state enforcement machinery
 *   - Liberty Advocacy Movement: Organized resistance (organized/constrained) — coordinates opposition; derives identity and power from continued crisis; has agency but faces high exit cost
 *   - Jurisdictional Sovereignty Advocates: Structural exit pathway (organized/mobile) — some jurisdictions phase out mandates; provides scaffold exit mechanism
 *   - Emergency Powers Doctrine: Institutional apparatus (institutional/arbitrage) — maintains mandate authority through formulaic legal reasoning; undergoes institutional decay (piton perspective)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing a political position as categorical natural law (false summit)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.78).
domain_priors:suppression_score(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.85).
domain_priors:theater_ratio(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, snare).
narrative_ontology:human_readable(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, "Vaccine Mandate Legitimacy (Bodily Autonomy Primacy Reading)").
narrative_ontology:topic_domain(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, "public_health_policy/constitutional_law/bioethics").

domain_priors:requires_active_enforcement(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, '6b3f5cfe-2b51-4632-8bb2-bcdb3df799f6').
narrative_ontology:cs_kernel_codification('6b3f5cfe-2b51-4632-8bb2-bcdb3df799f6', formalized).
narrative_ontology:cs_authority_grounding('6b3f5cfe-2b51-4632-8bb2-bcdb3df799f6', extraction).
narrative_ontology:cs_reading_relation('6b3f5cfe-2b51-4632-8bb2-bcdb3df799f6', vaccine_mandate_legitimacy__public_health_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('6b3f5cfe-2b51-4632-8bb2-bcdb3df799f6', vaccine_mandate_legitimacy__risk_stratification_reading, forecloses).
narrative_ontology:cs_axiom('6b3f5cfe-2b51-4632-8bb2-bcdb3df799f6', foundational, bodily_autonomy_categorical_absolute).
narrative_ontology:cs_axiom_status(bodily_autonomy_categorical_absolute, holdable).
narrative_ontology:cs_axiom_grounding('6b3f5cfe-2b51-4632-8bb2-bcdb3df799f6', bodily_autonomy_categorical_absolute, deontological).
narrative_ontology:cs_axiom('6b3f5cfe-2b51-4632-8bb2-bcdb3df799f6', foundational, coercive_legitimacy_requires_consent).
narrative_ontology:cs_axiom_status(coercive_legitimacy_requires_consent, holdable).
narrative_ontology:cs_axiom_grounding('6b3f5cfe-2b51-4632-8bb2-bcdb3df799f6', coercive_legitimacy_requires_consent, deontological).
narrative_ontology:cs_reference_frame('6b3f5cfe-2b51-4632-8bb2-bcdb3df799f6', liberal_consent_based_authority).
narrative_ontology:cs_drift_state('6b3f5cfe-2b51-4632-8bb2-bcdb3df799f6', contemporary_normalization_of_mandate, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6b3f5cfe-2b51-4632-8bb2-bcdb3df799f6', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vaccine_mandate_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, public_health_authorities).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vaccine_refusers).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, immunocompromised_vulnerable_population).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VACCINE REFUSER (SNARE) — Faces coercive choice: accept injection or lose employment, education access, or participation in public life. No material exit option. The constraint extracts behavioral compliance through elimination of alternatives. Maximum experienced suppression — the state monopolizes legitimate participation pathways.
constraint_indexing:constraint_classification(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: IMMUNOCOMPROMISED VULNERABLE (SNARE) — Bears asymmetric exposure risk from unvaccinated populations while also facing coercion if medical exemptions are denied or narrowly construed. Trapped between two extraction mechanisms: coercive mandate (state) and denial of protection (unvaccinated population). Trapped by medical dependency and lack of political voice. Maximum experienced extraction with no exit.
constraint_indexing:constraint_classification(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: PUBLIC HEALTH AUTHORITY (ROPE) — Experiences the constraint as a coordination mechanism: the mandate solves the collective action problem of disease transmission by establishing a clear behavioral standard. Benefits from institutional authority consolidation and ability to leverage state enforcement machinery. Arbitrage exit option — can shift policy without losing power. Net beneficiary position.
constraint_indexing:constraint_classification(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LIBERTY ADVOCACY MOVEMENT (TANGLED ROPE) — Organized resistance to bodily autonomy violation; coordinates collective refusal and legal challenge. Genuine coordination function (mobilizing opposition, sustaining identity through shared principle). Also bears extraction: movement leadership derives power from continued crisis; institutional repression increases solidarity and fundraising. Constrained exit — leaving the movement means accepting the mandate or social isolation. Moderate extractiveness because the movement has agency and can articulate exit paths (though costly).
constraint_indexing:constraint_classification(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: JURISDICTIONAL SOVEREIGNTY (SCAFFOLD) — Some jurisdictions reject mandates entirely; others implement conditional exemptions; others phase them out after emergency period. The constraint functions as a temporary coordination mechanism during crisis (vaccination coverage above herd immunity threshold) with explicit or implicit sunset. As vaccination rates increase and pandemic threat diminishes, mandate authority erodes. Exit path: jurisdictional variation and sunset clauses provide structural exit. Low effective extraction due to visible exit strategy.
constraint_indexing:constraint_classification(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: EMERGENCY POWERS DOCTRINE (PITON) — The legal/institutional apparatus that authorizes vaccine mandates (emergency powers, police powers for public health) has become ritualistic. The doctrine persists through institutional inertia even as emergency conditions fade. Courts validate mandates through formulaic application of precedent rather than scrutinizing the actual emergency justification. Theater ratio high (0.38 may underestimate — actual emergency scrutiny is minimal). The doctrine maintains extractive force through performative legal reasoning rather than live justification.
constraint_indexing:constraint_classification(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational universal perspective, this reading claims that bodily autonomy is a categorical natural right that admits of no exceptions and no state override. This is presented as a fixed feature of human dignity, not a contingent institutional arrangement. However, the structural data (clear beneficiaries in public health authorities, extractive mechanisms dependent on enforcement infrastructure, contingent on state capacity) reveals this as a false summit — the reading naturalizes a specific political/institutional stance rather than describing a genuine natural law.
constraint_indexing:constraint_classification(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, TR),
    TR >= 0.70.

:- end_tests(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): High, reflecting the coercive elimination of alternatives and the state's consolidation of institutional power over bodily integrity decisions. The measurement trajectory (0.65→0.78→0.82) shows extraction accumulation over the interval: as pandemic emergency threat diminishes, the mandate persists and intensifies, indicating the constraint is shifting from temporary emergency coordination toward permanent institutional extraction. This trajectory is diagnostically significant — extraction should decline as emergency conditions fade; instead it rises, suggesting the emergency exception has become normalized. Suppression (0.85): Very high. The constraint operates by eliminating alternatives: refusing vaccination excludes agents from employment, education, and public participation. This is not persuasion or incentive; it is suppression through elimination of legitimate exit routes. Suppression remains elevated even as emergency conditions fade, indicating the constraint's suppressive force is independent of the original justification. Theater ratio (0.38): Moderate-low. This reading does not claim the mandate is performative theater — the coercive apparatus is functionally operative. The theater component reflects the justificatory narrative: officials invoke emergency conditions and public health necessity, but the structural data suggests the emergency justification is becoming increasingly performative as conditions change (omega variable 'emergency_exceptionalism_duration'). Theater rises slightly over the interval as the gap between original emergency rationale and actual conditions widens.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full perspectival spectrum from a single set of base properties. The vaccine refuser sees pure extraction (Snare) — coercion with no coordination benefit. The immunocompromised vulnerable see dual extraction (Snare from both directions). The public health authority sees coordination (Rope) — the mandate solves a legitimate collective action problem. The liberty movement sees mixed extraction and resistance (Tangled Rope) — the system both constrains and mobilizes them. Jurisdictional sovereignty advocates see a temporary problem with exit paths (Scaffold) — mandates can be phased out without institutional collapse. The emergency powers doctrine sees its own degraded ritual (Piton) — legal authority persists through formulaic reasoning despite changed conditions. The analytical observer risks seeing a natural law (Mountain) — bodily autonomy as categorical — but the structural data reveals this reading naturalizes a political position: identifiable beneficiaries, engineered suppression, contingent enforcement, extraction accumulating as emergency justification fades. The perspectival gap reveals that the core disagreement is not empirical (about disease transmission or vaccine efficacy) but about the foundational axiom: whether bodily autonomy is categorical (this reading) or graduated (public health primacy reading) or risk-stratified (stratification reading).
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by the agent's structural relationship to the extraction flow. Vaccine refusers face a coercive mechanism (d→1.0, trapped, powerless) that eliminates their alternatives; they experience maximum extractiveness. The immunocompromised vulnerable face asymmetric harm from both the mandate (if exemptions are denied) and unvaccinated populations (if exemptions are honored); they are trapped between extraction mechanisms. The public health authority benefits from institutional consolidation and enforcement machinery (d→0.0, beneficiary, institutional); they experience the constraint as coordination. Liberty advocates have organized resistance capacity but high exit cost (constrained exit, d→0.65); they experience moderate-to-high extractiveness. Jurisdictional sovereignty advocates have structural options (mobile exit, d→0.55); they experience the constraint as a temporary coordination problem with exit paths. The emergency powers doctrine maintains authority through institutional inertia (piton, d→0.0 beneficiary); the doctrine has no exit cost because it persists regardless of conditions. The analytical observer's mountain classification risks naturalizing a political position as a natural law; this is a false summit because the constraint's beneficiaries, extraction mechanism, and suppression apparatus are all contingent on institutional structures, not immutable features of nature.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by acknowledging that extractiveness > 0.70 requires explicit resolution. The mandatrophy is resolved by reading context: this is one reading of a contested kernel. The bodily autonomy primacy reading FORECLOSES the public health primacy reading within a single framework (you cannot simultaneously hold that bodily autonomy is categorically impermissible to override AND that public health necessity can legitimately justify override). However, the readings COEXIST across different political factions in the actual world — different jurisdictions and institutions instantiate different readings. The mandatrophy resolution does not settle the factual dispute but clarifies the structure: the dispute is over foundational axioms (categorical vs graduated autonomy), not over empirical facts about disease transmission. Empirical evidence on vaccine efficacy, disease prevalence, or immunocompromised harm cannot resolve which reading is 'correct' because the readings rest on different deontological axioms. The mandatrophy is resolved by recognizing this structural distinction and admitting that the constraint cannot be classified to a single type — it is a snare to refusers, a rope to authorities, a tangled rope to organizers, a scaffold to jurisdictions with exit paths, a piton to the doctrine, and a false summit to observers who naturalize the political dispute.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    categorical_vs_graduated_autonomy,
    'Is bodily autonomy a categorical absolute or a graduated principle that admits proportional state override in genuine emergencies?',
    'Comparative constitutional law analysis; tracking which jurisdictions maintain absolute autonomy claims vs those permitting override under specified conditions; longitudinal assessment of whether ''genuine emergency'' criteria remain justifiable as conditions change',
    'If categorical: this reading holds; public health authority cannot legitimately mandate vaccines under any circumstance. If graduated: this reading forecloses, and risk-stratification or public-health-primacy readings become viable. This is the foundational conceptual divide.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(categorical_vs_graduated_autonomy, conceptual, 'Whether bodily autonomy is categorical or graduated').

omega_variable(
    immunocompromised_harm_asymmetry,
    'Who bears the extraction burden: vaccine refusers (coerced compliance), or immunocompromised populations (exposed to unvaccinated risk)? Is one form of harm categorically worse than the other?',
    'Epidemiological analysis of actual harm: mortality/morbidity from COVID-19 in unvaccinated vs vaccinated populations; tracking of immunocompromised outcomes under different mandate regimes; empirical assessment of whether exemptions + risk mitigation actually protect vulnerable populations',
    'If vaccine refusal harm >> mandate harm: this reading holds. If harm is symmetric or inverted: victim classification shifts to include vaccine refusers as primary victims, and the snare perspective for refusers is reinforced. If immunocompromised harm >> refusal harm: the public health reading becomes dominant, and this reading becomes a secondary voice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(immunocompromised_harm_asymmetry, empirical, 'Comparative harm burden between coercive mandate and unvaccinated exposure risk').

omega_variable(
    emergency_exceptionalism_duration,
    'How long is the emergency exception valid? If the original emergency condition (pandemic threat) recedes below some threshold, does the mandate lose its legitimacy?',
    'Tracking of disease prevalence, hospitalization rates, excess mortality over time; definition of ''emergency threshold'' below which mandates should lapse; historical comparison with other emergency powers (wartime conscription, martial law) and their sunset mechanisms',
    'If emergency must be continuous: perpetual mandates lose legitimacy once conditions improve, and the scaffold perspective (temporary coordination with sunset) becomes accurate. If emergency can be invoked retrospectively: perpetual mandates retain legitimacy regardless of conditions, and the snare classification hardens. This determines whether the constraint is genuinely temporary (scaffold) or permanently extractive (snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emergency_exceptionalism_duration, empirical, 'Duration of legitimate emergency exception for bodily autonomy override').

omega_variable(
    committer_kernel_reading_contest,
    'This constraint is one reading of the vaccine mandate legitimacy kernel. Three readings are held by different factions: bodily autonomy primacy (this one), public health primacy (sibling), and risk stratification (sibling). Which reading''s foundational axioms are overridden by empirical evidence or by drift in the political/institutional context?',
    'Mapping axiom grounding types (empirical vs deontological vs conventional) across siblings; tracking empirical drift (do pandemic conditions actually support original emergency framing?); documenting political/institutional drift (has the mandate moved from emergency temporary measure to routine public health tool?)',
    'If this reading''s axioms are empirically challenged: omega ''immunocompromised_harm_asymmetry'' resolution shifts toward public health reading. If institutional drift moves mandate from emergency temporary to routine: scaffold perspective gains ground. If both axioms remain holdable: coexists_with relations hold and the dispute remains unresolved.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_kernel_reading_contest, conceptual, 'Kernel reading contest: which foundational axioms are overridden or foreclosed').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vaxmand_theater_2020, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(vaxmand_theater_2021, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 6, 0.32).
narrative_ontology:measurement(vaxmand_theater_2022, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 12, 0.38).

% Extraction over time
narrative_ontology:measurement(vaxmand_extractiveness_2020, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(vaxmand_extractiveness_2021, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 6, 0.78).
narrative_ontology:measurement(vaxmand_extractiveness_2022, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 12, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(vaxmand_suppression_2020, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(vaxmand_suppression_2021, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 6, 0.85).
narrative_ontology:measurement(vaxmand_suppression_2022, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 12, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vaccine_mandate_legitimacy__public_health_primacy_reading).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vaccine_mandate_legitimacy__risk_stratification_reading).

% DUAL FORMULATION NOTE:
% The vaccine mandate legitimacy kernel decomposes into three structurally distinct constraints corresponding to three readings. Each reading instantiates a different ε value and different victim/beneficiary structure. The bodily autonomy primacy reading (ε=0.78, Snare) forecloses the public health primacy reading within a single theoretical framework but coexists with it across different jurisdictions and institutional contexts. These are not observables of a single constraint but three separate constraints sharing a contested kernel. Linked via network.affects_constraints to enable comparative analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
