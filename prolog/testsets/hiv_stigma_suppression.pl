% ============================================================================
% CONSTRAINT STORY: hiv_stigma_suppression
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hiv_stigma_suppression, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hiv_stigma_suppression
 *   human_readable: HIV Stigma and Suppression of Disclosure
 *   domain: public_health/social/identity
 *
 * SUMMARY:
 *   HIV stigma operates as a pure extraction mechanism disguised as moral and
 *   public health necessity. The constraint suppresses disclosure of HIV
 *   status through both material barriers (employment discrimination,
 *   healthcare denial, legal criminalization) and internalized shame
 *   (identity fusion with the diagnosis, internalized worthlessness). The
 *   suppression serves institutional interests — religious and moral
 *   authority institutions extract social control value; risk-denial
 *   frameworks extract legitimacy by externalizing disease responsibility
 *   onto the stigmatized population; public health institutions extract
 *   enforcement capacity through mandatory disclosure threats.
 *   Simultaneously, suppression directly undermines stated public health
 *   objectives by preventing testing, treatment initiation, and prevention
 *   behavior. The constraint is a diagnostic exemplar of how extraction can
 *   be perpetuated through a cover story of necessity ('we must stigmatize
 *   for public health') that is empirically false. The theater ratio (0.62)
 *   reflects that institutional HIV responses increasingly consist of
 *   performative moral gatekeeping — quarantine and exclusion ceremonies,
 *   mandatory testing with forced disclosure, public shaming — long after
 *   scientific evidence invalidated their disease control function. The
 *   trajectory shows both extractiveness and theater increasing over the
 *   interval (0.42 → 0.68 extractiveness; 0.45 → 0.62 theater), indicating
 *   institutional capture and degradation rather than public health
 *   improvement.
 *
 * KEY AGENTS:
 *   - People living with HIV: Primary victims (powerless/identity-locked and trapped) — face material barriers to disclosure (employment, healthcare, housing discrimination) and internalized suppression through identity fusion. Carry suppression regardless of structural barriers due to cognitive capture.
 *   - Risk-denial institutions: Primary beneficiaries (institutional/arbitrage) — extract legitimacy and social control by externalizing disease responsibility onto the infected population. Benefit from stigma-based moral authority.
 *   - Religious and cultural norm-setters: Secondary beneficiaries (institutional/arbitrage) — extract social cohesion through group identity maintenance (us vs. them) and moral authority claims. Stigma coordinates group boundaries.
 *   - Public health institutions: Complex actors (moderate/constrained) — have genuine coordination functions (treatment cascade, partner notification, prevention) but extract enforcement capacity through suppression and mandatory disclosure threats. Mixed rope-snare character.
 *   - At-risk populations (sex workers, LGBTQ+, people who inject drugs): Secondary victims (moderate/constrained) — face suppression through association with HIV, stigma generalization, and exclusion from prevention resources. Constrained by both material barriers and stigmatic assumptions.
 *   - Medical professionals with disclosure obligations: Institutional actors caught between confidentiality obligations and mandatory reporting laws. Experience institutional contradiction (constrained) — legal duty to maintain confidentiality conflicts with public health reporting mandates that operate through threat of forced disclosure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hiv_stigma_suppression, 0.68).
domain_priors:suppression_score(hiv_stigma_suppression, 0.75).
domain_priors:theater_ratio(hiv_stigma_suppression, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hiv_stigma_suppression, extractiveness, 0.68).
narrative_ontology:constraint_metric(hiv_stigma_suppression, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(hiv_stigma_suppression, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hiv_stigma_suppression, snare).
narrative_ontology:human_readable(hiv_stigma_suppression, "HIV Stigma and Suppression of Disclosure").
narrative_ontology:topic_domain(hiv_stigma_suppression, "public_health/social/identity").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hiv_stigma_suppression, stigma_perpetuators).
narrative_ontology:constraint_beneficiary(hiv_stigma_suppression, risk_denial_institutions).
narrative_ontology:constraint_victim(hiv_stigma_suppression, people_living_with_hiv).
narrative_ontology:constraint_victim(hiv_stigma_suppression, public_health_efficacy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% A person living with HIV faces structural suppression through material barriers (employment discrimination, housing denial, healthcare access restrictions) AND internalized suppression through identity fusion. The individual cannot disclose without risking livelihoods and relationships, but also cannot imagine themselves outside the HIV-positive identity they have constructed — secrecy becomes constitutive of self-concept. Exit from the constraint would require becoming a different person, not just changing circumstances.
constraint_indexing:constraint_classification(hiv_stigma_suppression, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% Material barriers to disclosure are insurmountable within the immediate context: employment-at-will settings where disclosure risks termination with no legal recourse; healthcare access conditional on disclosure to insurers who may deny coverage; family structures where disclosure risks abandonment or violence. The structural entrapment is independent of identity — the agent perceives the constraint as purely external and unchangeable.
constraint_indexing:constraint_classification(hiv_stigma_suppression, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Public health agencies coordinate HIV prevention and treatment through surveillance, partner notification, and treatment adherence programs — genuine coordination functions. Simultaneously, they extract compliance through threat of disclosure (breach of medical confidentiality), mandatory reporting to authorities, and selective resource allocation. They experience the constraint as both coordination necessity and enforcement burden.
constraint_indexing:constraint_classification(hiv_stigma_suppression, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Religious institutions, media gatekeepers, and cultural norm-setters benefit from HIV stigma through mechanisms of social control and moral authority. The constraint coordinates group identity ('us' vs. 'them') while extracting from the stigmatized outgroup. These beneficiaries experience the constraint as coordination of shared values and cultural coherence.
constraint_indexing:constraint_classification(hiv_stigma_suppression, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Institutional stigmatization rituals — mandatory testing with forced disclosure, quarantine protocols, social exclusion ceremonies — persist through institutional inertia long after their public health function has been scientifically invalidated. The theater ratio is high (0.62): much of institutional HIV response involves performative moral cleansing rather than functional disease control. These rituals maintain credibility through spectacle, not effectiveness.
constraint_indexing:constraint_classification(hiv_stigma_suppression, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% From a civilizational analytical view, HIV stigma functions as a pure extractive constraint: it suppresses disclosure (reducing public health efficacy), concentrates costs on the infected population (barriers to treatment, employment, relationships), and produces no coordination benefit that could not be achieved through non-stigmatizing mechanisms. Suppression is near-total: fear of disclosure operates through internalized shame and material consequences.
constraint_indexing:constraint_classification(hiv_stigma_suppression, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hiv_stigma_suppression_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hiv_stigma_suppression, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hiv_stigma_suppression, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hiv_stigma_suppression, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hiv_stigma_suppression, TR),
    TR >= 0.70.

:- end_tests(hiv_stigma_suppression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts from people living with HIV through suppression that prevents disclosure, impedes treatment access, blocks economic participation, and breaks social bonds. It extracts legitimacy from institutions through a cover story of necessity ('stigma is regrettable but necessary for disease control'). The extractiveness score reflects that suppression produces asymmetric costs (borne by the infected population) with no offsetting benefit for that population. Suppression (0.75): Very high. Multiple reinforcing suppression mechanisms operate: (1) Material barriers — employment-at-will settings allow termination upon disclosure; healthcare systems require disclosure to insurers who may deny coverage; housing and adoption laws explicitly discriminate; (2) Legal penalties — many countries criminalize HIV non-disclosure, creating disclosure risks; (3) Internalized shame — decades of stigmatizing public health messaging have internalized diagnosis as moral failing; (4) Relationship risks — disclosure often triggers abandonment, violence, or family fracture. Suppression approaches totality: most people living with HIV never disclose to all potential contacts, creating ongoing secrecy burden. Theater ratio (0.62): Moderate-high and rising. Institutional HIV responses include genuine disease control components (antiretroviral therapy, testing programs, prevention education) but increasingly emphasize performative moral gatekeeping: quarantine theaters (historically), mandatory testing with threat of forced disclosure, public health campaigns that conflate disease with moral failing. The theater has increased over time as scientific evidence on undetectable-untransmittable (U=U) status shifted the discourse — institutions have responded by increasing performative enforcement (stricter disclosure laws, moral panic around criminalization) rather than reducing stigma-based interventions.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival divergence. The powerless/identity-locked perspective sees pure snare; the institutional beneficiary sees rope or even coordination necessity; the public health actor sees mixed tangled rope; the analytical observer sees extraction with a false cover story. The gap reveals how suppression operates: beneficiaries experience it as natural/necessary; targets experience it as all-consuming. The identity-lock mechanism is critical: even when structural barriers are removed, the identity-locked target continues to experience suppression because their self-concept is fused with the diagnosis. This is the signature of cognitive capture masquerading as individual pathology.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation for each perspective reflects power, exit options, and beneficiary/victim status. A person living with HIV classified as powerless/identity-locked faces d ≈ 0.89 (high target status, internalized suppression prevents exit recognition). The same person classified as powerless/trapped faces d ≈ 0.95 (full target status, material barriers absolute). Beneficiary institutions (religious/cultural authorities) classified as institutional/arbitrage face d ≈ 0.05 (full beneficiary, can exit constraint at will, benefits from coordination value). Public health institutions classified as moderate/constrained face d ≈ 0.55 (symmetric position: some benefits from coordination function, some costs from enforcement burden, constrained by law and funding). The directionality chain produces f(d) values that amplify extraction for targets and dampen extraction for beneficiaries, creating the perspectival gap visible in classification divergence.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION — The constraint resolves the mandatrophy by identifying a false necessity narrative. The cover story is 'stigma is necessary for public health' (treating the snare as rope or mountain — justified extraction). The structural analysis shows: (1) Suppression serves institutional interests, not disease control. (2) Non-stigmatizing public health approaches (Scandinavian decriminalization + treatment-as-prevention) achieve superior outcomes. (3) Beneficiaries are not disease control experts but moral authority institutions. Therefore, the constraint is pure extraction masquerading as coordination necessity. The mandatrophy is resolved by decomposing the false unity: the legitimate public health coordination function (treatment, prevention, testing) should be separated from the extraction mechanism (stigma, suppression, criminalization). These are two different constraints: one is rope or tangled rope with genuine coordination; the other is snare with a false necessity cover story.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_vs_structural_entrapment,
    'What proportion of disclosure suppression is driven by internalized stigma (identity-locked) vs. material barriers (trapped/constrained)?',
    'Longitudinal study of barriers to disclosure: qualitative interviews post-disclosure comparing pre- and post-disclosure fear levels; analysis of whether material barriers alone (legal employment protection, healthcare access guaranteed) eliminate suppression or whether internalized shame persists independently',
    'If primarily identity-locked (>60%): intervention focus must be cognitive/identity-reframing alongside structural removal of material barriers. If primarily trapped (>60%): material policy change (employment protection, healthcare access, confidentiality guarantees) would substantially reduce suppression. Mixed mechanism suggests dual intervention required.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_structural_entrapment, empirical, 'Proportion of suppression driven by internalized identity-lock vs structural material barriers').

omega_variable(
    suppression_persistence_after_structural_removal,
    'If all material barriers to disclosure are removed (employment protection, healthcare access, legal confidentiality), does internalized suppression persist and at what magnitude?',
    'Comparison of disclosure rates and psychological outcomes in jurisdictions with strong legal protections (Canada, Netherlands, Scandinavia) vs. weak protections (US at-will employment states, countries with criminalization). Measurement of residual shame/secrecy even when legal barriers are absent.',
    'If suppression persists at >40% magnitude even with structural removal: the constraint has significant identity-lock component that requires cognitive intervention (community building, narrative reframing, identity affiliation). If suppression drops to <20%: structural removal is sufficient and identity-lock is secondary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_persistence_after_structural_removal, empirical, 'Whether internalized suppression persists after removal of material barriers').

omega_variable(
    coordination_function_necessity,
    'Could public health coordination objectives (partner notification, treatment adherence, prevention cascade) be achieved through mechanisms that do not rely on stigmatizing suppression?',
    'Case study analysis of non-stigmatizing public health approaches (opt-in partner notification, community-based treatment adherence support, shame-free prevention education); measurement of treatment cascade outcomes in countries with decriminalization and de-stigmatization policies vs. criminalization regimes',
    'If non-stigmatizing coordination achieves equivalent or superior outcomes: stigma suppression has zero coordination justification and is pure extraction. If non-stigmatizing approaches perform worse: some degree of stigmatic pressure may have coordination function (though it may still be extractively excessive).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_function_necessity, empirical, 'Whether public health coordination requires stigmatizing suppression or can be achieved through non-stigmatizing mechanisms').

omega_variable(
    theater_threshold_for_institutional_degradation,
    'At what theater ratio (currently 0.62) does institutional HIV response fully become degraded ritual with no functional disease control component?',
    'Analysis of institutional response components: segregate actual disease control activities (testing, treatment, prevention) from performative activities (public shaming, quarantine theater, moral gatekeeping). Measure functional outcome variance explained by each component.',
    'If functional component explains >40% of outcome variance: theater at current ratio is concerning but institution retains mixed rope/piton character. If <20%: institution is fully piton (degraded ritual maintained by inertia). If functional component is negative (stigma reduces treatment-seeking, delays care, drives transmission): institution is pure extraction with negative public health function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_threshold_for_institutional_degradation, empirical, 'Theater threshold at which institutional HIV response becomes fully degraded ritual').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hiv_stigma_suppression, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hiv_stigma_tr_t0, hiv_stigma_suppression, theater_ratio, 0, 0.45).
narrative_ontology:measurement(hiv_stigma_tr_t8, hiv_stigma_suppression, theater_ratio, 8, 0.54).
narrative_ontology:measurement(hiv_stigma_tr_t16, hiv_stigma_suppression, theater_ratio, 16, 0.62).
narrative_ontology:measurement(hiv_stigma_tr_t24, hiv_stigma_suppression, theater_ratio, 24, 0.62).

% Extraction over time
narrative_ontology:measurement(hiv_stigma_be_t0, hiv_stigma_suppression, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(hiv_stigma_be_t8, hiv_stigma_suppression, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(hiv_stigma_be_t16, hiv_stigma_suppression, base_extractiveness, 16, 0.68).
narrative_ontology:measurement(hiv_stigma_be_t24, hiv_stigma_suppression, base_extractiveness, 24, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hiv_stigma_suppression, identity_coordination).
narrative_ontology:affects_constraint(hiv_stigma_suppression, healthcare_access_barriers).
narrative_ontology:affects_constraint(hiv_stigma_suppression, criminalization_of_disclosure).

% DUAL FORMULATION NOTE:
% HIV stigma suppression decomposes into two structurally distinct constraints: (1) identity_coordination_stigma (ε≈0.55) — the cognitive capture mechanism through which identity becomes fused with diagnosis, enabling internalized suppression even when material barriers are removed; (2) material_suppression_mechanisms (ε≈0.72) — employment discrimination, healthcare denial, legal criminalization that create structural barriers to disclosure. These are linked: material barriers reinforce identity lock, and identity lock persists after material barriers are removed. Both must be addressed for meaningful suppression reduction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hiv_stigma_suppression, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
