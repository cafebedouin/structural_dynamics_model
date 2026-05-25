% ============================================================================
% CONSTRAINT STORY: uk_national_security_doctrine
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_uk_national_security_doctrine, []).

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
 *   constraint_id: uk_national_security_doctrine
 *   human_readable: UK National Security Doctrine Framework
 *   domain: geopolitical/institutional_governance
 *
 * SUMMARY:
 *   The UK National Security Doctrine represents a structural tension between
 *   the coordinating function of collective defense (genuine public good) and
 *   the extractive expansion of state surveillance and executive power. The
 *   doctrine emerged as a coherent framework consolidating Cold War
 *   institutional practices, post-9/11 threat responses, and post-Brexit
 *   assertion of sovereign security autonomy. It operates simultaneously as a
 *   rope (coordinating NATO/Five Eyes partnerships, organizing defense
 *   resources), a tangled rope (extracting privacy and democratic constraint
 *   while providing security coordination), a snare (entangling citizens in
 *   asymmetric surveillance), a piton (maintaining performative parliamentary
 *   oversight despite functional degradation), and a scaffold (under
 *   constraint from international human rights frameworks with declining
 *   force post-Brexit). The constraint exhibits the full perspectival range:
 *   intelligence agencies see pure coordination; citizens see pure
 *   extraction; Parliament sees its own institutional degradation; human
 *   rights law sees a temporary constraint on an upward extraction
 *   trajectory. The extractiveness trajectory (0.38 → 0.58 over 20 years)
 *   shows monotonic accumulation of asymmetric power, reflecting post-9/11
 *   institutional expansion (RIPA 2000 → Investigatory Powers Act 2016) and
 *   post-Brexit sovereignty assertion unbound by EU legal constraints. The
 *   theater_ratio increase (0.45 → 0.62) indicates parliamentary oversight
 *   has become increasingly performative even as its surveillance burden has
 *   grown.
 *
 * KEY AGENTS:
 *   - Intelligence Agencies (GCHQ, MI5, MI6): Primary beneficiaries (institutional/arbitrage) — doctrine legitimizes expansion of collection capabilities, budget growth, and operational autonomy
 *   - Defense Establishment: Primary beneficiary (institutional/arbitrage) — doctrine justifies military modernization, NATO commitments, AUKUS partnership
 *   - British Citizens: Primary victims (powerless/trapped) — bear suppression of privacy and autonomy; cannot exit national security state
 *   - Democratic Governance Model: Victim/identity-locked (powerless/identity_locked) — UK self-concept as functional democracy persists while oversight power atrophies
 *   - Civil Liberties Organizations: Mixed actor (moderate/constrained) — benefit from institutional relevance; constrained by legal barriers to challenging security decisions
 *   - Five Eyes Alliance Partners: Mixed institutional actor (organized/constrained) — coordinate intelligence burden; extract/bear asymmetric collection costs depending on partner size
 *   - Parliamentary Oversight Bodies (ISC, SIAC, IPT): Performative constraint (institutional/arbitrage) — institutional inertia maintains oversight theater despite degraded enforcement capacity
 *   - Investigative Journalism: Temporary constraint (organized/constrained) — scaffold function declining under suppression (Official Secrets Act, source protection barriers)
 *   - International Human Rights Framework (ECHR, UN): Declining external constraint (organized/constrained) — post-Brexit sovereignty removes international standard-setting force
 *   - Analytical Observer: Risk of naturalizing contingency — may treat security imperative as immutable law rather than policy choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(uk_national_security_doctrine, 0.58).
domain_priors:suppression_score(uk_national_security_doctrine, 0.65).
domain_priors:theater_ratio(uk_national_security_doctrine, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(uk_national_security_doctrine, extractiveness, 0.58).
narrative_ontology:constraint_metric(uk_national_security_doctrine, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(uk_national_security_doctrine, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(uk_national_security_doctrine, tangled_rope).
narrative_ontology:human_readable(uk_national_security_doctrine, "UK National Security Doctrine Framework").
narrative_ontology:topic_domain(uk_national_security_doctrine, "geopolitical/institutional_governance").

domain_priors:requires_active_enforcement(uk_national_security_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(uk_national_security_doctrine, intelligence_agencies).
narrative_ontology:constraint_beneficiary(uk_national_security_doctrine, defense_establishment).
narrative_ontology:constraint_beneficiary(uk_national_security_doctrine, security_industrial_complex).
narrative_ontology:constraint_victim(uk_national_security_doctrine, civil_liberties_framework).
narrative_ontology:constraint_victim(uk_national_security_doctrine, transparency_accountability).
narrative_ontology:constraint_victim(uk_national_security_doctrine, public_electoral_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BRITISH CITIZEN (SNARE) — Subject to expanded surveillance, data collection, and security profiling with no meaningful exit. Citizens cannot opt out of the national security apparatus. They bear suppression (restricted privacy, communication monitoring, algorithmic targeting) with minimal coordination benefit. The doctrine constrains mobility and autonomy without genuine consent mechanisms.
constraint_indexing:constraint_classification(uk_national_security_doctrine, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DEMOCRATIC GOVERNANCE (SNARE via IDENTITY_LOCKED) — The UK's identity as a democratic polity with oversight and accountability has become identity-locked within the security doctrine. Parliament sees itself as unable to meaningfully constrain security operations; executive claims of 'ongoing operations' and 'threat level' are accepted as self-justifying. The democratic identity persists performatively while substantive constraint power atrophies. Exit would require abandoning the UK's self-concept as a working democracy.
constraint_indexing:constraint_classification(uk_national_security_doctrine, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 3: CIVIL LIBERTIES ORGS (TANGLED ROPE) — Benefit from the existence of a security framework (which provides them targets for advocacy and organizational relevance); also bear extraction through suppressed transparency, limited access to evidence, and legal barriers to challenging security decisions. Constrained exit: they can litigate but within frameworks the doctrine defines. Genuine coordination function exists (defining security boundaries) alongside asymmetric extraction.
constraint_indexing:constraint_classification(uk_national_security_doctrine, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: INTELLIGENCE AGENCIES (ROPE) — Primary beneficiaries. Experience the doctrine as pure coordination: it legitimizes and coordinates intelligence operations, resource allocation, and international partnerships. Exit options abundant (could operate under different legal frameworks but gain no benefit from exiting). The doctrine functions as their organizing principle — low effective extraction because they designed it.
constraint_indexing:constraint_classification(uk_national_security_doctrine, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: DEFENSE ESTABLISHMENT (ROPE) — Primary beneficiary. Doctrine legitimizes military capabilities, procurement, and strategic positioning. Net beneficiary through expanded budgets and strategic autonomy. Experiences the constraint as coordination of UK defense posture and NATO/AUKUS alignment. Low experienced extraction due to arbitrage options and beneficiary status.
constraint_indexing:constraint_classification(uk_national_security_doctrine, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: FIVE EYES ALLIANCE (TANGLED ROPE) — Coordinate intelligence sharing and burden of security monitoring (genuine coordination function). Also extract asymmetric intelligence advantage — US has superior collection and processing capacity; smaller partners (Canada, Australia, UK) gain access but at cost of reduced privacy standards and operational autonomy. Constrained exit: leaving Five Eyes brings intelligence isolation cost.
constraint_indexing:constraint_classification(uk_national_security_doctrine, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: PARLIAMENTARY OVERSIGHT (PITON) — Institutions (ISC, SIAC, IPT) designed to constrain security operations are now largely performative. Theater ratio 0.62 reflects that oversight committees receive sanitized briefings, lack enforcement mechanisms against agencies, and operate within doctrine frameworks they nominally constrain. The oversight machinery persists through institutional inertia despite degraded constraint function. Agencies shape what Parliament sees; Parliament sees degraded process but cannot exit without undermining democratic legitimacy performance.
constraint_indexing:constraint_classification(uk_national_security_doctrine, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: INTL HUMAN RIGHTS FRAMEWORK (SCAFFOLD) — European human rights law, UN conventions, and international norms create temporary constraints on UK security doctrine. Sunset logic is real: UK post-Brexit is building independent rights framework (ECHR replacement expected 2026-2028). The constraint of international alignment is temporary and declining. Exit path visible (UK legal sovereignty recovery). Suppression is high (governments restrict rights in security name) but declining.
constraint_indexing:constraint_classification(uk_national_security_doctrine, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 9: INVESTIGATIVE JOURNALISM (SCAFFOLD) — Organized media scrutiny constrains security operations through exposure, but the constraint is temporary and fragile. Sunset is conditional: high suppression (Official Secrets Act, D-Notice system, source protection barriers) limits investigation depth. If journalistic capacity declines or surveillance of journalists increases, this scaffold collapses. Constrained exit: journalists can leave UK or self-censor but face career/legitimacy cost.
constraint_indexing:constraint_classification(uk_national_security_doctrine, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 10: ANALYTICAL OBSERVER / NATURALIZED VIEW (MOUNTAIN) — From a civilizational perspective, national security doctrine is treated as immutable law of statecraft: all states must defend borders, collect intelligence, suppress threats. This perspective sees the doctrine as an unchangeable consequence of anarchic international system. However, this naturalizes what is actually a contingent institutional arrangement shaped by Cold War structures, post-9/11 threat perception, and surveillance technology development. The engine will flag this as a false summit — the 'inevitable' framing obscures policy choices.
constraint_indexing:constraint_classification(uk_national_security_doctrine, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(uk_national_security_doctrine_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(uk_national_security_doctrine, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(uk_national_security_doctrine, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(uk_national_security_doctrine, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(uk_national_security_doctrine, TR),
    TR >= 0.70.

:- end_tests(uk_national_security_doctrine_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The doctrine extracts asymmetric power (surveillance, restricted information access, executive autonomy) from citizens and democratic institutions while providing legitimate coordination function (collective defense, intelligence partnership). The extraction is not total (0.70+) because genuine security coordination benefits exist and exit options exist at high cost but not zero cost. The trajectory shows accumulation: baseline 0.38 reflects pre-9/11 Cold War institutional framework; expansion to 0.58 reflects post-9/11 doctrinal shift and post-Brexit sovereignty assertion. Suppression (0.65): High. Citizens face restricted privacy rights, communication monitoring, expanded detention powers, and restricted access to classified information justifying restrictions. Parliament faces restricted briefing access and structural inability to enforce oversight. Media faces Official Secrets Act constraints and D-Notice system. However, suppression is not total (0.85+) because exit options exist: emigration is possible (at cost), legal challenge through courts remains available, international scrutiny persists (declining). Theater ratio (0.62): Moderate-high. Parliamentary oversight (ISC, SIAC, IPT) maintains legitimacy performance while enforcement capacity has degraded. Agencies manage what information Parliament accesses; parliamentary recommendations often go unimplemented. Official secrecy prevents public accountability mechanisms. However, theater is not extreme (0.85+) because some genuine constraint remains: court cases do succeed, investigative journalism does expose abuses, parliamentary inquiries do force disclosure of historical violations. Theater has increased over interval as surveillance complexity has outpaced oversight capacity.
 *
 * PERSPECTIVAL GAP:
 *   The doctrine demonstrates maximum perspectival divergence. Intelligence agencies classify as rope (they designed the coordination function and experience only benefits). Citizens classify as snare (maximum extraction, suppression, no exit). Parliament classifies as piton (sees its own institutional degradation but maintains performative function). Civil liberties organizations classify as tangled rope (both benefit and bear extraction from rights protection work in a rights-suppressing regime). International human rights law classifies as declining scaffold (Brexit removes external standard-setting force). The analytical observer risks classifying as mountain (security necessity as immutable law) but the structural data reveals this as naturalization of contingent institutional arrangements shaped by historical threat perceptions and budget politics. The perspectives converge only on suppression (0.65) as high — all agree exit is costly — but diverge radically on whether the suppression serves legitimate public goods (intelligence agencies, defense) or extracts asymmetric power (citizens, civil liberties organizations).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from beneficiary/victim declarations and exit options. Intelligence agencies (beneficiaries, arbitrage exit) have d ≈ 0.05 (full beneficiary) and experience low/negative χ. Citizens (victims, trapped exit) have d ≈ 0.95 (full target) and experience high χ. Parliamentary oversight (victims, constrained exit) has d ≈ 0.75 and experiences moderate χ. Five Eyes partners (mixed: beneficiary in intelligence access, victim in asymmetric collection burden) have differentiated d values depending on partner capacity: UK has higher d (more collection burden) than US (lower collection burden). Civil liberties organizations (mixed: benefit from advocacy role, bear extraction from rights suppression) have d ≈ 0.60. The analytical observer (analytical exit, neutral structural position) has d ≈ 0.72 (canonical for analytical context) and experiences moderate χ but with epistemic advantage of seeing the full structure. The directionality spread (0.05 to 0.95) is maximum, reflecting structural misalignment: doctrine serves the interests of powerful institutional beneficiaries at cost to powerless dispersed victims.
 *
 * MANDATROPHY ANALYSIS:
 *   The UK National Security Doctrine illustrates how classification resolves under mandatrophy by showing the tension between coordinating function (legitimate public good of collective defense) and extractive expansion (asymmetric power accumulation in executive and intelligence bureaucracies). The mandate is security: defend the realm, coordinate intelligence, maintain NATO/Five Eyes partnerships. The atrophy is democratic constraint: parliamentary oversight capability has degraded relative to security operations scope; transparency and accountability have declined; exit options have narrowed through surveillance expansion. The classification system reveals that both the rope and snare aspects are structurally real — the doctrine simultaneously coordinates defense and extracts power. The mandatrophy is not resolved by picking one type but by acknowledging the presheaf: from the intelligence agency perspective (institutional/arbitrage), it is rope; from the citizen perspective (powerless/trapped), it is snare; from the analytical perspective (civilizational scope), it is tangled rope with rising χ and declining constraint capacity. The doctrine's extractiveness trajectory (0.38 → 0.58) shows accumulation of asymmetric power extraction over the interval, with no corresponding increase in democratic accountability mechanisms. The theater ratio increase (0.45 → 0.62) shows that oversight has become increasingly performative even as surveillance capability has expanded. This is diagnostic of mandatrophy: the original mandate (security coordination) persists while constraint mechanisms (parliamentary oversight, rights frameworks, transparency) have atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threat_perception_construction,
    'To what extent is the current UK security doctrine responsive to genuine threat assessment versus performatively constructed to justify existing institutional interests and budgets?',
    'Comparative analysis: correlation between threat-level claims and actual measured attack/incident rates; declassified post-action reviews of threat forecasts; intelligence budget trajectory versus documented threat evolution',
    'If doctrine is threat-responsive: classification shifts toward legitimate rope/scaffold. If doctrine is institutional theater: classification remains snare/piton with higher confidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threat_perception_construction, empirical, 'Whether UK security doctrine responds to threat or constructs it').

omega_variable(
    surveillance_chilling_effect_magnitude,
    'What is the measurable impact of UK surveillance doctrine on political speech, activism, and electoral autonomy? Is it significant enough to constitute democratic capture or does it remain within acceptable suppression bounds?',
    'Longitudinal analysis: correlation between surveillance expansion (RIPA updates, Investigatory Powers Act) and declines in protest participation, FOIA requests, journalistic investigations; polling on self-censorship; comparative analysis with democracies with lower surveillance suppression',
    'If chilling effect is measured as high: victim classification of electoral autonomy is confirmed (snare). If effect is marginal: suppression metric should be reduced and some perspectival classifications downshifted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(surveillance_chilling_effect_magnitude, empirical, 'Magnitude of surveillance chilling effect on political speech and activism').

omega_variable(
    parliamentary_oversight_enforcement_capacity,
    'Does UK parliamentary oversight of security (ISC, SIAC, IPT) represent genuine constraint or performative theater? Can Parliament actually force agencies to modify operations or is oversight purely informational?',
    'Case analysis of ISC recommendations: frequency of agency non-compliance, timeline from ISC finding to operational change, resource allocation to oversight bodies versus security budgets, access to evidence and classified information',
    'If oversight is genuine: piton classification is incorrect and should be rope. If oversight is theater: piton classification confirmed and may warrant reclassification as snare from democratic governance perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(parliamentary_oversight_enforcement_capacity, empirical, 'Whether parliamentary oversight enforces constraint or remains performative').

omega_variable(
    five_eyes_asymmetry_quantification,
    'How asymmetric is intelligence sharing within Five Eyes? Does UK benefit from partnership or primarily serve as signal collection platform for larger partners?',
    'Declassified documents on intelligence sharing protocols; analysis of bilateral intelligence dependence; comparison of signal intelligence collection burden versus disseminated intelligence value; countries'' strategic autonomy in major security decisions post-intelligence consultation',
    'If asymmetry is high: Five Eyes perspective is snare not tangled_rope. If balanced: tangled_rope confirmed and extraction moderates.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(five_eyes_asymmetry_quantification, empirical, 'Asymmetry of intelligence sharing in Five Eyes partnership').

omega_variable(
    post_brexit_rights_framework_convergence,
    'Will UK post-ECHR rights framework actually constrain security doctrine or will sovereignty recovery enable further security expansion unchecked by international standards?',
    'Proposed Bills of Rights text; parliamentary debate on security exceptions; comparison with precedent from Commonwealth countries that exited international human rights frameworks; government security policy statements during transition period',
    'If new framework constrains security: international human rights scaffold remains active. If new framework enables expansion: scaffold collapses and snare classification becomes dominant.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(post_brexit_rights_framework_convergence, preference, 'Whether post-ECHR UK framework will constrain or enable security expansion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(uk_national_security_doctrine, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uknsd_tr_t0, uk_national_security_doctrine, theater_ratio, 0, 0.45).
narrative_ontology:measurement(uknsd_tr_t10, uk_national_security_doctrine, theater_ratio, 10, 0.58).
narrative_ontology:measurement(uknsd_tr_t20, uk_national_security_doctrine, theater_ratio, 20, 0.62).
narrative_ontology:measurement(uknsd_tr_t5, uk_national_security_doctrine, theater_ratio, 5, 0.5).

% Extraction over time
narrative_ontology:measurement(uknsd_be_t0, uk_national_security_doctrine, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(uknsd_be_t10, uk_national_security_doctrine, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(uknsd_be_t20, uk_national_security_doctrine, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(uknsd_be_t5, uk_national_security_doctrine, base_extractiveness, 5, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(uk_national_security_doctrine, enforcement_mechanism).
narrative_ontology:affects_constraint(uk_national_security_doctrine, five_eyes_intelligence_asymmetry).
narrative_ontology:affects_constraint(uk_national_security_doctrine, post_brexit_rights_framework).
narrative_ontology:affects_constraint(uk_national_security_doctrine, parliamentary_oversight_degradation).
narrative_ontology:affects_constraint(uk_national_security_doctrine, uk_immigration_securitization).

% DUAL FORMULATION NOTE:
% The UK National Security Doctrine is upstream of several constrained by its expansion: Five Eyes partnership asymmetry is structured by UK doctrine; post-Brexit rights framework is constrained by doctrine's sovereignty assertions; parliamentary oversight degradation follows from doctrine's expansion of executive security claims; immigration policy securitization is justified within doctrine's threat framing. Each downstream constraint has distinct ε but shares common doctrinal root.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(uk_national_security_doctrine, institutional, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
