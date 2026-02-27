% ============================================================================
% CONSTRAINT STORY: the_wall_procedural_barrier
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_the_wall_procedural_barrier, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: the_wall_procedural_barrier
 *   human_readable: The Intelligence/Law Enforcement Information Sharing Barrier ("The Wall")
 *   domain: legal/institutional
 *
 * SUMMARY:
 *   The 'Wall' refers to procedural and statutory restrictions on information
 *   sharing between U.S. intelligence agencies (CIA, FBI Intelligence
 *   Division, NSA) and criminal prosecutors (DOJ Criminal Division) that were
 *   in effect from approximately 1995 to 2001. The wall emerged from Fourth
 *   Amendment jurisprudence and statutory interpretation: criminal trials
 *   require discovery, cross-examination, and adversarial testing, which
 *   expose investigative sources and methods; intelligence operations require
 *   operational security and compartmentalization. The wall was meant to
 *   preserve both protections by maintaining organizational and procedural
 *   boundaries. However, the constraint exhibits fundamental tension between
 *   its coordination function (protecting sources, preventing discovery
 *   abuse, preserving investigative secrecy) and its extraction function
 *   (restricting prosecutorial access to intelligence data, slowing
 *   counterterrorism response, enabling information asymmetry within the
 *   executive branch). This constraint is a diagnostic exemplar of tangled
 *   rope-snare-scaffold dynamics at the inter-institutional level.
 *
 * KEY AGENTS:
 *   - Intelligence Agencies (CIA/NSA/FBI Intelligence Division): Institutional beneficiary (institutional/arbitrage) — experiences wall as protective coordination; can maintain parallel investigative channels; shields sources and methods from criminal discovery
 *   - Terrorism Investigation Teams: Primary victim (powerless/trapped) — field agents face hard barriers to accessing intelligence data; cannot exit; suffer operational paralysis and delayed response
 *   - Prosecutor's Office (DOJ Criminal Division): Secondary victim and beneficiary (organized/constrained) — benefits from confidentiality preservation (rope-like) but suffers extraction from inability to access intelligence (snare-like); constrained exit through statutory barrier
 *   - Attorney General / Executive Authority: Powerful institutional actor (powerful/mobile) — has discretionary authority to modify wall procedures; exhibits both coordination (institutional authority) and extraction (control of information flow)
 *   - Civil Liberties Coalition: Organized advocate (organized/constrained) — sees wall as temporary sunset-clause protecting fourth amendment norms; constrained by post-2001 reforms
 *   - DOJ Institutional Structure: Institutional actor (institutional/arbitrage) — maintains wall as theater; the practical barrier has weakened while institutional scaffolding persists (piton perspective)
 *   - Analytical Observer: Universal/civilizational level (analytical/analytical) — risks naturalizing contingent institutional design as inherent to democratic governance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(the_wall_procedural_barrier, 0.52).
domain_priors:suppression_score(the_wall_procedural_barrier, 0.68).
domain_priors:theater_ratio(the_wall_procedural_barrier, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(the_wall_procedural_barrier, extractiveness, 0.52).
narrative_ontology:constraint_metric(the_wall_procedural_barrier, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(the_wall_procedural_barrier, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(the_wall_procedural_barrier, tangled_rope).
narrative_ontology:human_readable(the_wall_procedural_barrier, "The Intelligence/Law Enforcement Information Sharing Barrier (\"The Wall\")").
narrative_ontology:topic_domain(the_wall_procedural_barrier, "legal/institutional").

domain_priors:requires_active_enforcement(the_wall_procedural_barrier).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(the_wall_procedural_barrier, intelligence_agencies).
narrative_ontology:constraint_beneficiary(the_wall_procedural_barrier, fourth_amendment_protection).
narrative_ontology:constraint_victim(the_wall_procedural_barrier, criminal_prosecution_effectiveness).
narrative_ontology:constraint_victim(the_wall_procedural_barrier, counterterrorism_response).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TERRORISM INVESTIGATION TEAM (SNARE) — Field agents tracking suspected terrorist cells face hard barriers to accessing wiretap data or intelligence summaries from parallel intelligence investigations. The wall is not advisory; it is enforced through legal sanctions and career consequences. Investigators cannot exit: the barrier is embedded in statute and agency protocol. They bear extraction through operational paralysis, delayed response capability, and inability to connect dots across investigative silos. Maximum experienced suppression — alternatives are absent.
constraint_indexing:constraint_classification(the_wall_procedural_barrier, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PROSECUTOR'S OFFICE (TANGLED ROPE) — Prosecutors benefit from the wall's coordination function: it preserves confidentiality standards and minimizes information contamination between investigation types, reducing suppression motion exposure. But they also suffer extraction: they cannot access intelligence data that would support prosecutions, and they bear coordination costs of maintaining separate investigative workflows. Constrained exit — prosecutors cannot unilaterally breach the wall without legal exposure. Mixed coordination (confidentiality preservation) and extraction (reduced prosecutorial tools).
constraint_indexing:constraint_classification(the_wall_procedural_barrier, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INTELLIGENCE AGENCY (ROPE) — The CIA/NSA/FBI Intelligence Division experiences the wall primarily as coordination: it preserves the ability to conduct human intelligence operations and signals interception without exposure to criminal discovery rules. The wall creates a functional separation that protects sources and methods. Arbitrage exit: intelligence agencies can maintain parallel investigative channels and can petition for exceptions through the AG. Net beneficiary — the wall's restrictions flow away from intelligence, toward law enforcement.
constraint_indexing:constraint_classification(the_wall_procedural_barrier, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CIVIL LIBERTIES COALITION (SCAFFOLD) — Privacy advocates and civil liberties organizations see the wall as a temporary coordination mechanism with a sunset: the intent was to prevent law enforcement fishing expeditions into intelligence data. As surveillance capabilities expanded post-2001, and as new coordination protocols (the FISA Amendment Act, the Patriot Act's provisions on information sharing) created alternative pathways, the original barrier's function was partially subsumed. The coalition sees the wall as a sunset clause protecting fourth amendment norms — but the Patriot Act reforms (effective 2001) began circumventing the original restrictions. Scaffold classification derives from both the sunset logic and the declining theater ratio as alternative mechanisms replace the barrier.
constraint_indexing:constraint_classification(the_wall_procedural_barrier, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: DOJ INSTITUTIONAL STRUCTURE (PITON) — The wall becomes a theatrical institutional boundary. DOJ maintains the internal wall (between Criminal Division prosecutors and National Security Division prosecutors) as a structural marker of institutional identity and perceived legal constraint, even as the Patriot Act and later FISA Amendments progressively weakened it. The actual function (protecting sources and preventing discovery abuse) has atrophied; the performance (maintaining separate divisions, separate filing systems, separate briefings) persists. Theater ratio rises over the interval as the practical barrier declines but institutional scaffolding remains.
constraint_indexing:constraint_classification(the_wall_procedural_barrier, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ATTORNEY GENERAL / EXECUTIVE AUTHORITY (TANGLED ROPE) — The AG has both coordination incentives (preserving confidentiality and source protection, managing institutional boundaries) and extraction incentives (controlling information flow, gating access to classified material, maintaining hierarchical authority). Mobile exit: the AG can unilaterally modify wall procedures through guidance, and has done so repeatedly (notably in 2001 under Ashcroft). This perspective combines the rope-like benefit (institutional authority over information) with snare-like constraint (subordinate agencies' compliance). The power atom 'powerful' reflects the AG's structural position as superior institutional actor.
constraint_indexing:constraint_classification(the_wall_procedural_barrier, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some separation between intelligence gathering and criminal prosecution is inherent to law: criminal procedure requires discovery, trial rights, and adversarial testing; intelligence operations require secrecy, compartmentalization, and protection of sources. This perspective sees the wall as reflecting an immutable tension in democratic institutions. However, the structural data contradicts the mountain classification — the wall was a specific historical policy choice (circa 1995), not a logical necessity. The engine will compute this as a false summit, revealing that 'institutional necessity' naturalizes what is actually contingent institutional design.
constraint_indexing:constraint_classification(the_wall_procedural_barrier, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(the_wall_procedural_barrier_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(the_wall_procedural_barrier, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(the_wall_procedural_barrier, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(the_wall_procedural_barrier, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(the_wall_procedural_barrier, TR),
    TR >= 0.70.

:- end_tests(the_wall_procedural_barrier_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The wall creates measurable prosecution delays and denies prosecutors access to potentially actionable intelligence. However, the extraction is not as severe as a pure snare (ε > 0.66) because: (a) prosecutors can petition the AG for wall breaches in appropriate cases, (b) intelligence agencies have incentive to cooperate (even if reluctant), and (c) alternative investigation pathways exist, though they are slower. The moderate-high value reflects both the real prosecution costs and the fact that some exit channels exist. Suppression (0.68): High. The barriers are statutorily embedded (Classified Information Procedures Act, FISA, Executive Order 12333) and institutionally enforced through career consequences and legal exposure. Investigators cannot casually bypass the wall. However, suppression is not total (0.85+) because the AG can modify wall procedures, and exceptions exist for emergency situations. Theater ratio (0.58): Moderate-high. The wall has both functional and performative aspects. The functional aspect: genuinely protecting classified sources and methods reduces discovery burden. The performative aspect: the wall persists through institutional inertia even as the Patriot Act (2001) and later FISA Amendments progressively weakened the original restrictions. By 2001, the formal barrier remained but the practical effect was diminishing — theater ratio rises as the performative component (institutional boundary maintenance) grows while functional protection declines.
 *
 * PERSPECTIVAL GAP:
 *   This constraint is a paradigmatic case of perspectival divergence without observable variation. All six types are legitimate readings of the same structural data from different institutional positions. The snare is the field agent's structural reality (trapped, no exit, high extraction). The rope is the intelligence agency's experience (protective coordination, arbitrage exit). The tangled rope is the prosecutor's mixed experience (both beneficial and extractive). The scaffold is the civil liberties advocate's sunset hypothesis (temporary, being replaced by Patriot Act reforms). The piton is the DOJ's institutional performance (performative boundaries, atrophying function). The mountain is the analytical observer's risk (naturalizing contingent design). No single type is 'correct' — the presheaf of perspectives over the institutional terrain IS the structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is derived from the agent's structural position relative to the information flow and decision authority. Intelligence agencies (beneficiaries with arbitrage exit) experience low d → low/negative χ. Terrorism investigation teams (victims with trapped exit) experience high d → high χ. Prosecutors occupy the middle: they are both beneficiaries (confidentiality protection) and victims (information denial), with constrained but non-zero exit options (d ≈ 0.55). The AG (powerful actor with mobile exit) experiences lower d than trapped agents because the AG can unilaterally modify wall procedures. The civil liberties coalition (organized but constrained) experiences moderate d reflecting their advocacy power but institutional constraints. The directionality derivation is sensitive to exit options: trapped agents experience maximum effective extraction; arbitrage agents experience minimum; constrained agents experience middle values. The AG's mobile exit option (can modify wall through guidance) is the critical structural difference from prosecutors' constrained exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The wall resolves mandatrophy by showing that high extractiveness (0.52) combined with genuine coordination function (source protection, confidentiality preservation) and required enforcement (statutory + institutional + career consequences) produces legitimate Tangled Rope classification. Mandatrophy would arise if the constraint were classified as pure Snare despite its genuine protective coordination benefits. The wall is extractive (high suppression, operational paralysis) AND coordinative (source protection, discovery protection) — this is not a mislabeling. The piton classification (institutional theater) is valid from the DOJ's institutional perspective because theater ratio has risen above 0.5 as the practical barrier has weakened. The scaffold classification (civil liberties sunset) is valid because the Patriot Act (2001) began creating alternative information-sharing pathways. The snare classification (field agents) is valid because trapped investigators experience pure extraction. The analytical mountain is a false summit — the engine's detector identifies naturalization of contingent design. The mandatrophy is structurally resolved: the constraint is genuinely tangled (mixed coordination and extraction), and perspectival classifications correctly reflect each agent's experience without conflating types.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_for_threat_level,
    'What threat level justifies breaching information-sharing barriers — and who decides?',
    'Retrospective analysis of actual threat assessments (pre-9/11 vs post-9/11) and their correlation to subsequent attack evidence; institutional review of AG guidance and FISA Court threshold decisions',
    'If threshold is low: wall is pure extraction (snare dominates). If threshold is high: wall provides legitimate protective function (rope/scaffold dominates). If threshold is unclear: executive overreach and institutional capture risk (piton/tangled_rope dominates).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_for_threat_level, conceptual, 'What threat level justifies breaching information barriers').

omega_variable(
    sources_and_methods_vulnerability,
    'Do intelligence sources and methods actually require wall-level protection, or is the wall used to shield investigative techniques from legal scrutiny?',
    'Comparative analysis of actual compromises and intelligence failures traceable to discovery disclosure; audit of classified material successfully protected under wall procedures vs material over-classified for institutional convenience',
    'If sources genuinely vulnerable: wall provides protective benefit (rope/scaffold). If over-classified for discretion: wall is purely extractive institutional control (snare/piton).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sources_and_methods_vulnerability, empirical, 'Whether wall genuinely protects sources or shields institutional discretion').

omega_variable(
    prosecution_effectiveness_cost,
    'What is the quantifiable prosecution and counterterrorism cost of the wall? Does it prevent convictions, or does it prevent weak prosecutions?',
    'Archival analysis of DoJ criminal referral rejection rates and stated reasons pre/post 2001; case studies of missed connections (e.g., 9/11 intelligence-prosecution link failures); comparison to prosecutorial effectiveness in allied nations without equivalent walls',
    'If high cost (material prosecution failures): snare classification dominates, wall is extraction. If low cost (blocks marginally-viable cases): rope/scaffold dominates, wall is protective coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prosecution_effectiveness_cost, empirical, 'Quantifiable prosecution effectiveness cost of the wall').

omega_variable(
    fourth_amendment_substitution,
    'Does the wall function as a genuine fourth amendment protection mechanism, or is it a performance artifact while bulk surveillance expands outside the criminal/intelligence boundary?',
    'Comparative analysis of actual fourth amendment suppression motions granted vs denied pre/post-wall (1995-2001); examination of alternative surveillance expansion (NSA bulk programs, financial surveillance, metadata collection) that bypassed the wall entirely',
    'If wall actually protects fourth amendment rights: rope/scaffold classifications valid. If wall is theater while rights erode elsewhere: piton/snare dominates — the wall is performative protection while extraction (surveillance expansion) proceeds through other mechanisms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fourth_amendment_substitution, empirical, 'Whether wall functions as genuine privacy protection or performative while actual surveillance expands').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(the_wall_procedural_barrier, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wall_tr_t0, the_wall_procedural_barrier, theater_ratio, 0, 0.42).
narrative_ontology:measurement(wall_tr_t3, the_wall_procedural_barrier, theater_ratio, 3, 0.5).
narrative_ontology:measurement(wall_tr_t6, the_wall_procedural_barrier, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(wall_be_t0, the_wall_procedural_barrier, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(wall_be_t3, the_wall_procedural_barrier, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(wall_be_t6, the_wall_procedural_barrier, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(the_wall_procedural_barrier, enforcement_mechanism).
narrative_ontology:affects_constraint(the_wall_procedural_barrier, fisa_surveillance_expansion).
narrative_ontology:affects_constraint(the_wall_procedural_barrier, executive_information_control).
narrative_ontology:affects_constraint(the_wall_procedural_barrier, criminal_discovery_asymmetry).

% DUAL FORMULATION NOTE:
% The Wall is a specific institutional barrier (1995-2001). It decomposes into two distinct structural constraints: (1) procedural information-sharing restriction (ε=0.52, Tangled Rope), and (2) institutional theater/performance boundary (ε=0.35, Piton). These are not two measurements of the same constraint but two structurally distinct claims. The first concerns the actual prosecutorial and counterterrorism costs of separation; the second concerns the institutional performance of maintaining boundaries whose practical function has attenuated. The first is more empirically tractable; the second is more institutional/political. They are linked because institutional theater depends on the barrier persisting, while practical extraction depends on the barrier's real-world effect.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
