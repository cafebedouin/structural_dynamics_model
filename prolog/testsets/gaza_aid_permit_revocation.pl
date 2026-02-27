% ============================================================================
% CONSTRAINT STORY: gaza_aid_permit_revocation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gaza_aid_permit_revocation, []).

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
 *   constraint_id: gaza_aid_permit_revocation
 *   human_readable: Revocation of Work Permits for Local Aid Workers in Gaza
 *   domain: political/humanitarian_access
 *
 * SUMMARY:
 *   The revocation of work permits for local Palestinian staff of
 *   international humanitarian organizations in Gaza represents a structural
 *   extraction mechanism operating through administrative control. Israeli
 *   military administration (COGAT) controls permit issuance and revocation
 *   for all personnel working in humanitarian organizations operating within
 *   occupied Gaza. Beginning in late 2023 and intensifying through 2024-2025,
 *   systematic permit revocations have eliminated employment for Palestinian
 *   aid workers, forcing organizations to either reduce operations, employ
 *   expensive international replacements (with reduced local knowledge and
 *   access), or halt programs entirely. The constraint exhibits the
 *   characteristics of a pure snare from the perspective of Palestinian aid
 *   workers and the dependent Gaza population: no exit options, maximum
 *   suppression (no alternative employment, blockade conditions), and high
 *   extraction (loss of livelihood, reduced aid access). From the Israeli
 *   military perspective, the mechanism appears as legitimate security
 *   coordination. From the international community perspective, it is a
 *   partially enforceable but degraded humanitarian protection mechanism
 *   (piton). The constraint's theater ratio (0.45) reflects that permit
 *   revocations are justified primarily through security screening rationales
 *   that may or may not have actual security function, obscuring the
 *   extraction mechanism beneath ostensible regulatory authority.
 *
 * KEY AGENTS:
 *   - Palestinian Aid Workers: Primary victims (powerless/trapped) — lose employment and livelihood with zero alternatives due to Gaza blockade and economic collapse
 *   - International Humanitarian Organizations: Secondary victims (moderate/constrained) — forced to reduce operations or employ expensive international replacements; cannot exit Gaza without abandoning mandate
 *   - Gaza Population: Indirect victims (powerless/trapped) — dependent on aid delivery; reduced aid worker capacity means reduced aid access and increased humanitarian emergencies
 *   - Israeli Military Administration (COGAT): Primary beneficiary (institutional/arbitrage) — controls aid worker access; extracts operational control and political leverage over humanitarian sector
 *   - International Community / Human Rights Monitors: Organized observers (organized/constrained) — document violations but have limited enforcement capacity; dependent on Israeli cooperation for access and information
 *   - International Humanitarian Law Framework: Institutional structure (institutional/arbitrage in theory) — provides legal framework for humanitarian protection but enforcement is degraded (piton)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing occupation-based control as inherent and immutable rather than contingent policy choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gaza_aid_permit_revocation, 0.68).
domain_priors:suppression_score(gaza_aid_permit_revocation, 0.75).
domain_priors:theater_ratio(gaza_aid_permit_revocation, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gaza_aid_permit_revocation, extractiveness, 0.68).
narrative_ontology:constraint_metric(gaza_aid_permit_revocation, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(gaza_aid_permit_revocation, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gaza_aid_permit_revocation, snare).
narrative_ontology:human_readable(gaza_aid_permit_revocation, "Revocation of Work Permits for Local Aid Workers in Gaza").
narrative_ontology:topic_domain(gaza_aid_permit_revocation, "political/humanitarian_access").

domain_priors:requires_active_enforcement(gaza_aid_permit_revocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gaza_aid_permit_revocation, israeli_military_administration).
narrative_ontology:constraint_victim(gaza_aid_permit_revocation, palestinian_aid_workers).
narrative_ontology:constraint_victim(gaza_aid_permit_revocation, humanitarian_organizations).
narrative_ontology:constraint_victim(gaza_aid_permit_revocation, gazan_population_access_to_aid).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PALESTINIAN AID WORKER (SNARE) — No meaningful exit. Permit revocation eliminates employment in humanitarian sector; alternative employment in Gaza is nonexistent due to blockade and economic collapse. Family depends on aid sector wages. Faces maximum coercive pressure with zero alternatives. Maximum experienced extraction.
constraint_indexing:constraint_classification(gaza_aid_permit_revocation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: INTERNATIONAL HUMANITARIAN ORGANIZATIONS (SNARE) — Cannot exit Gaza without abandoning mission; must operate within permit system controlled by Israeli military. Constrained exit: can relocate staff but cannot redirect mission. Faces extraction through forced employment of international staff (higher cost) or mission reduction. Suppression is structural: no alternative channels exist for aid delivery into Gaza.
constraint_indexing:constraint_classification(gaza_aid_permit_revocation, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ISRAELI MILITARY ADMINISTRATION (COGAT) (ROPE) — Experiences permit revocation as a coordination mechanism: controlling aid worker permits enables military security screening and operational control. From this perspective, the system solves a real coordination problem (vetting aid workers for security risks). Beneficiary with full arbitrage options — can revoke, reinstate, or modify permit conditions at will. Low effective extraction from own perspective; sees the mechanism as security coordination.
constraint_indexing:constraint_classification(gaza_aid_permit_revocation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INTERNATIONAL COMMUNITY / HUMAN RIGHTS MONITORS (TANGLED ROPE) — Has coordination function (documenting violations, applying diplomatic pressure) but also experiences extraction (limited ability to influence permit policy, dependent on Israeli cooperation for access). Can organize multilateral response but exits are constrained by geopolitical relationships. Sees mixed coordination-extraction structure.
constraint_indexing:constraint_classification(gaza_aid_permit_revocation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL HUMANITARIAN LAW FRAMEWORK (PITON) — Theoretically governs permit revocation through Geneva Conventions (protecting humanitarian access). Framework persists as institutional ritual despite selective enforcement and frequent violations. Theater ratio reflects gap between legal obligations and actual protection mechanisms. IHL exists and is cited but has degraded functional capacity to prevent permit revocations in practice.
constraint_indexing:constraint_classification(gaza_aid_permit_revocation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: GAZA POPULATION (INDIRECT VICTIM) (SNARE) — Cannot exit or organize response. Dependent on humanitarian aid for survival; aid worker permit revocations reduce aid delivery capacity. Bears extraction through reduced aid access, increased hunger, medical emergencies left untreated. No alternatives; no exit options. Maximum experienced harm with zero agency.
constraint_indexing:constraint_classification(gaza_aid_permit_revocation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / SOVEREIGNTY VIEW (MOUNTAIN) — From a civilizational/universal perspective, occupying powers have inherent authority to control entry/exit and permit issuance for populations under military control. Permit revocation is presented as an immutable consequence of military occupation status. However, the structural data reveals false summit: permit revocation is a contingent policy choice, not an inherent feature of occupation. The mountain classification naturalizes what is actually a discretionary extraction mechanism.
constraint_indexing:constraint_classification(gaza_aid_permit_revocation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gaza_aid_permit_revocation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gaza_aid_permit_revocation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gaza_aid_permit_revocation, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(gaza_aid_permit_revocation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(gaza_aid_permit_revocation, TR),
    TR >= 0.70.

:- end_tests(gaza_aid_permit_revocation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint produces direct economic extraction (loss of Palestinian worker income), operational extraction (forcing organizations to redirect resources), and instrumental extraction (gaining political control over humanitarian access). The trajectory from 0.52 to 0.68 reflects intensification: initial selective revocations (security-justified) evolved into systematic exclusion. Base extraction is legitimized through security rhetoric but functions structurally as employment elimination and humanitarian access control. Suppression (0.75): Very high. Palestinian aid workers face suppression through: (1) no alternative employment in Gaza (blockade conditions, economic collapse), (2) no ability to challenge permit decisions (military administrative authority), (3) family dependence on aid sector wages, (4) no exit or substitution options. Organizations face suppression through: (1) inability to operate without permits, (2) no alternative aid delivery channels, (3) choice between mission abandonment or cost escalation. Theater ratio (0.45): Moderate-low. Permit revocation justification relies on security screening rhetoric ('vetting for Hamas connections,' 'preventing aid diversion'), but the structural function is access control and employment elimination. The theater is lower than traditional bureaucratic mechanisms because the security justification is often transparent as secondary rationale; the primary function (control through permit authority) is visible. The ratio increases slightly over the interval as revocations become more systematic and less individually justified.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a 'Rope-Snare duality' — the same structural mechanism (permit control by military authority) is experienced as coordination by the beneficiary and as maximum extraction by victims. This gap is not a measurement ambiguity but a fundamental structural property: permit authority inherently centralizes control in one agent and removes alternatives for all others. The gap cannot be closed through better information or clarification — it reflects the irreducible asymmetry of permit-based access control. The piton perspective (IHL framework) shows that international humanitarian law nominally addresses this gap through humanitarian carve-outs and presumptive protection of aid access, but enforcement mechanisms are degraded, converting the legal framework into performative ritual.
 *
 * DIRECTIONALITY LOGIC:
 *   Palestinian aid workers: Structural position as victims with trapped exit options produces maximum directionality d ≈ 0.95, yielding f(d) ≈ 1.42 (powerless equivalent). They experience the full weight of extraction with zero agency or exit. Israeli military administration: Position as beneficiary with arbitrage exit options produces low directionality d ≈ 0.05-0.15, yielding negative or near-zero f(d) — they experience the constraint as coordination benefit (control gained), not extraction cost. International organizations: Position as moderate-power victims with constrained exit produces d ≈ 0.65-0.75, yielding f(d) ≈ 1.00-1.15 (moderate experienced extraction) — they can partially exit (relocate staff, change operations) but cannot fully exit without abandoning mandate. Gaza population: Position as powerless indirect victims with trapped options produces d ≈ 0.93, f(d) ≈ 1.40 — maximum experienced extraction despite not being direct permit holders. International community: Position as organized observers with analytical/constrained exit produces d ≈ 0.70-0.75, f(d) ≈ 1.12 — can document and pressure but cannot prevent, experiencing moderate extraction through limited agency.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY PRESENT AND REQUIRES RESOLUTION: The classification as Snare is clear from victim perspectives (Palestinian workers, Gaza population, organizations). However, the Israeli military administration's perspective generates the false Rope classification — the mandate to provide security coordination is real and legitimate, but it is being used as cover for extraction. The risk is conflating 'security coordination is a legitimate function' with 'therefore the mechanism is Rope rather than Snare.' Mandatrophy resolution requires recognizing that extraction mechanisms can be justified through legitimate mandates without becoming coordination mechanisms. The permit system serves BOTH coordination (security screening) AND extraction (employment elimination, political control). The mandatrophy is resolved by acknowledging that the classification depends on the extraction ratio: if security screening is the primary function and employment impact is secondary, the mechanism is Tangled Rope (hybrid). If employment control is primary and security screening is secondary justification, the mechanism is Snare with security theater. The intensity of permit revocations (0.52 → 0.68 extractiveness trajectory) and the breadth of victimization (Palestinian workers, organizations, Gaza population all trapped) suggest the constraint functions as Snare with security mandate (i.e., security coordination is the stated mandate but extraction is the structural outcome). The mandate is genuine but is being operationalized as extraction mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    security_vetting_necessity,
    'Are permit revocations genuinely necessary for security screening, or do they serve primarily as extraction/control mechanisms with minimal actual security function?',
    'Comparative analysis: security incidents involving permit holders vs. revoked workers; data on permit revocation grounds (percentage citing security vs. other administrative reasons); cross-case comparison with other occupied territories and conflict zones',
    'If genuine security necessity: constraint shifts toward Rope/Tangled Rope from some perspectives. If primarily extraction: Snare classification confirmed across all victim perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(security_vetting_necessity, empirical, 'Whether permit revocations serve genuine security functions').

omega_variable(
    alternative_aid_delivery_capacity,
    'Could international aid organizations maintain comparable aid delivery using only international (non-Palestinian) staff, or is Palestinian local staff functionally irreplaceable?',
    'Logistics analysis: cost differential for international vs. local staffing; operational capacity comparisons pre- and post-revocation; documented bottlenecks that cannot be overcome with international staff alone',
    'If international staff can substitute: suppression floor lowers, extraction becomes partial. If local staff irreplaceable: suppression remains high, confirms maximum extraction logic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_aid_delivery_capacity, empirical, 'Whether Palestinian aid workers are functionally replaceable by international staff').

omega_variable(
    permit_revocation_reversibility,
    'What is the historical pattern of permit reinstatement? Are revocations intended as permanent exclusion or temporary coercive pressure?',
    'Historical data: percentage of revoked permits reinstated; timeline of revocation-to-reinstatement; conditions for reinstatement; declarations by COGAT on permanence vs. temporality of revocations',
    'If commonly reinstated: constraint may have Scaffold element (temporary coercion with exit path). If permanent: pure Snare confirmed (no possibility of restored employment).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(permit_revocation_reversibility, empirical, 'Historical pattern of permit reinstatement vs. permanent revocation').

omega_variable(
    humanitarian_carve_out_enforceability,
    'Is the humanitarian carve-out in international law (presumptive protection of humanitarian access) structurally enforceable against permit revocation, or is it merely aspirational?',
    'Legal precedent analysis: cases where humanitarian carve-outs have successfully prevented permit-based access denial; documentation of enforcement mechanisms (sanctions, court orders, etc.); comparison to other territorial control scenarios',
    'If enforceable: Piton classification confirmed (legal framework exists but enforcement degraded). If purely aspirational: moves toward acknowledging IHL as theater, Piton confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(humanitarian_carve_out_enforceability, conceptual, 'Whether humanitarian carve-out in international law has structural enforceability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gaza_aid_permit_revocation, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gaza_permit_tr_t0, gaza_aid_permit_revocation, theater_ratio, 0, 0.35).
narrative_ontology:measurement(gaza_permit_tr_t6, gaza_aid_permit_revocation, theater_ratio, 6, 0.4).
narrative_ontology:measurement(gaza_permit_tr_t12, gaza_aid_permit_revocation, theater_ratio, 12, 0.45).

% Extraction over time
narrative_ontology:measurement(gaza_permit_be_t0, gaza_aid_permit_revocation, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(gaza_permit_be_t6, gaza_aid_permit_revocation, base_extractiveness, 6, 0.62).
narrative_ontology:measurement(gaza_permit_be_t12, gaza_aid_permit_revocation, base_extractiveness, 12, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gaza_aid_permit_revocation, enforcement_mechanism).
narrative_ontology:affects_constraint(gaza_aid_permit_revocation, gaza_humanitarian_access_blockade).
narrative_ontology:affects_constraint(gaza_aid_permit_revocation, palestinian_employment_collapse).
narrative_ontology:affects_constraint(gaza_aid_permit_revocation, international_ngo_operational_reduction).

% DUAL FORMULATION NOTE:
% The permit revocation mechanism is downstream of broader Gaza blockade constraints but represents a distinct structural lever. The blockade constrains all economic activity; permit revocation specifically targets humanitarian sector. These are separate constraints with different extractiveness profiles: blockade affects all Palestinians (broader but less directly controlled), while permit revocation affects aid workers specifically (narrower but more directly enforced through administrative mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gaza_aid_permit_revocation, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
