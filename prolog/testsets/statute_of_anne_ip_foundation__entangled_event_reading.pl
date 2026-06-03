% ============================================================================
% CONSTRAINT STORY: statute_of_anne_ip_foundation__entangled_event_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statute_of_anne_ip_foundation__entangled_event_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: statute_of_anne_ip_foundation__entangled_event_reading
 *   human_readable: The Statute of Anne as Entangled Conceptual-Institutional Event
 *   domain: legal_history/intellectual_property/institutional_economics
 *
 * SUMMARY:
 *   The Statute of Anne (1710) is typically framed as either a conceptual
 *   innovation (the birth of limited copyright as a tool for learning) or an
 *   institutional reallocation (rights shifted from the Stationers' Company
 *   to authors). This reading refuses that separation: the statute is a
 *   single event where conceptual change and institutional consolidation
 *   occurred as one inseparable action. The statute could not have introduced
 *   the concept of 'limited copyright' without simultaneously vesting rights
 *   in a specific institution (initially the stationers, nominally authors);
 *   conversely, the institutional arrangement could not be legitimated
 *   without the conceptual innovation of 'limited term' and 'author focus.'
 *   The entanglement is the constraint: neither the concept nor the
 *   institution can be extracted from the other without losing coherence. The
 *   beneficiary is ambiguous—nominally authors gained rights, but practically
 *   the stationers captured the institutional mechanisms (printing monopoly,
 *   distribution control, author dependence). The victim is conceptual
 *   clarity: any attempt to separate 'what copyright is' from 'who controls
 *   it' fails. The knowledge commons cannot exit because both dimensions lock
 *   simultaneously. The theater ratio rises over the first 50 years as the
 *   'limited right' narrative persists while institutional practice evolves
 *   toward perpetuity—the entanglement creates space for performative claims
 *   (term limits, learning exceptions) to coexist with institutional
 *   extraction (copyright lengthening).
 *
 * KEY AGENTS:
 *   - Stationers' Company: Practical institutional beneficiary (institutional/arbitrage) — monopoly consolidated and legitimated through IP concept; controls author intermediation
 *   - Authors (1710): Nominal rights-holders, actually identity-locked (powerless/identity_locked) — offered authorial identity through statute but trapped in stationer dependence; cannot claim rights without institutional gatekeeping
 *   - Knowledge Commons: Victim (powerless/trapped) — both conceptual and institutional dimensions seal access; no alternative framework available
 *   - Later Generations of Authors (post-1710): Moderate constrained agents (moderate/constrained) — benefit from limited-term concept but pay institutional cost of publisher gatekeeping
 *   - Conceptual Clarity: Victim (abstract, no power atom) — the entanglement prevents clean philosophical separation of what 'copyright' is from who controls it
 *   - Open Knowledge Coalition (modern): Organized alternative agents (organized/mobile) — building non-entangled frameworks (creative commons, open source) that separate concept from institutional monopoly
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statute_of_anne_ip_foundation__entangled_event_reading, 0.58).
domain_priors:suppression_score(statute_of_anne_ip_foundation__entangled_event_reading, 0.62).
domain_priors:theater_ratio(statute_of_anne_ip_foundation__entangled_event_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statute_of_anne_ip_foundation__entangled_event_reading, tangled_rope).
narrative_ontology:human_readable(statute_of_anne_ip_foundation__entangled_event_reading, "The Statute of Anne as Entangled Conceptual-Institutional Event").
narrative_ontology:topic_domain(statute_of_anne_ip_foundation__entangled_event_reading, "legal_history/intellectual_property/institutional_economics").

domain_priors:requires_active_enforcement(statute_of_anne_ip_foundation__entangled_event_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statute_of_anne_ip_foundation__entangled_event_reading, '4621840d-f4a4-49cc-aea5-137453dc6138').
narrative_ontology:cs_kernel_codification('4621840d-f4a4-49cc-aea5-137453dc6138', fixed_text).
narrative_ontology:cs_authority_grounding('4621840d-f4a4-49cc-aea5-137453dc6138', extraction).
narrative_ontology:cs_interpretation_layer_present('4621840d-f4a4-49cc-aea5-137453dc6138').
narrative_ontology:cs_reading_relation('4621840d-f4a4-49cc-aea5-137453dc6138', statute_of_anne_ip_foundation__conceptual_emergence_reading, coexists_with).
narrative_ontology:cs_reading_relation('4621840d-f4a4-49cc-aea5-137453dc6138', statute_of_anne_ip_foundation__institutional_reallocation_reading, influences).
narrative_ontology:cs_axiom('4621840d-f4a4-49cc-aea5-137453dc6138', foundational, concept_and_institution_co_constitute).
narrative_ontology:cs_axiom_status(concept_and_institution_co_constitute, holdable).
narrative_ontology:cs_axiom_grounding('4621840d-f4a4-49cc-aea5-137453dc6138', concept_and_institution_co_constitute, deontological).
narrative_ontology:cs_axiom('4621840d-f4a4-49cc-aea5-137453dc6138', foundational, entanglement_enables_invisible_extraction).
narrative_ontology:cs_axiom_status(entanglement_enables_invisible_extraction, holdable).
narrative_ontology:cs_axiom_grounding('4621840d-f4a4-49cc-aea5-137453dc6138', entanglement_enables_invisible_extraction, empirically_contingent).
narrative_ontology:cs_reference_frame('4621840d-f4a4-49cc-aea5-137453dc6138', limited_copyright_for_author_learning).
narrative_ontology:cs_drift_state('4621840d-f4a4-49cc-aea5-137453dc6138', contemporary_digital_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4621840d-f4a4-49cc-aea5-137453dc6138', '2026-02-27T14:32:00Z').
narrative_ontology:cs_kernel_id(statute_of_anne_ip_foundation__entangled_event_reading, statute_of_anne_ip_foundation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__entangled_event_reading, stationers_company).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__entangled_event_reading, publishers_practical_monopoly).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__entangled_event_reading, conceptual_clarity).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__entangled_event_reading, author_nominal_rights).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__entangled_event_reading, knowledge_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: KNOWLEDGE COMMONS (SNARE) — Trapped by the simultaneous locking of both concept and institution. Cannot access or develop alternative frameworks because the statute crystallizes copyright-as-property AND vests it in publishers simultaneously. The commons bears full extraction cost: idea-space is sealed, and institutional mechanisms prevent exit or alternative coordination.
constraint_indexing:constraint_classification(statute_of_anne_ip_foundation__entangled_event_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: AUTHORS / NOMINAL RIGHTS-HOLDERS (SNARE) — Offered nominally enhanced rights while the institutional reality remains captured by the Stationers' Company. Identity-locked through the authorial frame: to claim the benefits of authorship, one must accept the stationers' intermediation. The statute entangles the concept of author-as-rights-holder with institutional dependence on stationers. Exit would require abandoning the authorial identity altogether.
constraint_indexing:constraint_classification(statute_of_anne_ip_foundation__entangled_event_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 3: STATIONERS' COMPANY / PRACTICAL MONOPOLY (ROPE) — Experiences the statute as coordination mechanism that legitimates their existing monopoly. The entanglement is advantageous: the statute cannot be separated into pure concept (which might threaten monopoly) or pure reallocation (which might seem unfair). By fusing concept and institution, the statute launders monopoly as natural outcome of IP philosophy. Arbitrage opportunities abound through licensing control and author dependency.
constraint_indexing:constraint_classification(statute_of_anne_ip_foundation__entangled_event_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LATER AUTHORS / GENERATIONAL (TANGLED ROPE) — Constrained by the entanglement: they benefit from the conceptual innovation (copyright as limited right, term limits) but pay the institutional cost (dependence on publisher gatekeeping, stationer-mediated distribution). The statute enables authorship as a professional identity (coordination benefit) while extracting control of that identity through institutional mechanisms. Cannot separate the concept they benefit from the institution that constrains them.
constraint_indexing:constraint_classification(statute_of_anne_ip_foundation__entangled_event_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: THE 'LIMITED RIGHT' NARRATIVE (PITON) — The statute's official framing—copyright as a limited regulatory tool for learning, not perpetual property—persists performatively while institutional practice has evolved toward perpetuity. The narrative theater (term limits, learning exceptions) is increasingly decoupled from practice (copyright term extensions, fair use erosion). The conceptual framework maintains legitimacy through inertia even as institutions have departed from it.
constraint_indexing:constraint_classification(statute_of_anne_ip_foundation__entangled_event_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: OPEN KNOWLEDGE COALITION (SCAFFOLD) — Organized agents (open access movements, creative commons, digital libraries) see the entanglement as a temporary institutional lock-in with exit paths. The conceptual entanglement can be disentangled through new frameworks (open source principles, knowledge commons licensing). Sunset logic: alternative institutional arrangements (digital commons, non-proprietary coordination) are building parallel infrastructure that will render the 1710 entanglement obsolete.
constraint_indexing:constraint_classification(statute_of_anne_ip_foundation__entangled_event_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / INSTITUTIONAL DETERMINISM (MOUNTAIN) — From a civilizational perspective, this reading risks naturalizing the entanglement as inevitable: concepts and institutions necessarily co-constitute; you cannot have copyright-the-idea without copyright-the-law-and-institution. This perspective verges on a false summit—what appears immutable about institutional entanglement is actually a specific historical contingency.
constraint_indexing:constraint_classification(statute_of_anne_ip_foundation__entangled_event_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statute_of_anne_ip_foundation__entangled_event_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(statute_of_anne_ip_foundation__entangled_event_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(statute_of_anne_ip_foundation__entangled_event_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(statute_of_anne_ip_foundation__entangled_event_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(statute_of_anne_ip_foundation__entangled_event_reading, TR),
    TR >= 0.70.

:- end_tests(statute_of_anne_ip_foundation__entangled_event_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting the constraint's dual mechanism. The statute extracts through institutional monopoly capture (Stationers' Company continues dominating distribution and licensing) while the conceptual frame ('limited copyright for authors') legitimates that extraction as fair and natural. The extraction is not maximal because some coordination benefit is real—the concept of limited term is genuine institutional innovation. Suppression (0.62): Moderate-high. Suppression operates on two levels: institutionally through the stationer monopoly on printing and distribution; conceptually through the entanglement preventing alternative frameworks from becoming thinkable. Authors cannot organize outside the stationer system and simultaneously claim the rights the statute nominally grants them. Theater ratio (0.68): Moderately high, increasing over time. The 'limited right' narrative is performed (by 1760 the term limits and learning exceptions exist in law) while institutional practice diverges (stationers extend effective control through licensing, author agreements, and monopolistic practices). The entanglement creates room for theater because concept and institution reinforce each other—defenders of institutional monopoly invoke the limited-term concept; defenders of the concept invoke institutional necessity.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival divergence reveals the entanglement's function. The Stationers see coordination (Rope)—the statute legitimates their monopoly while claiming to benefit authors, solving a coordination problem they define. Nominal authors see extraction with identity-lock (Snare)—they are offered authorial identity contingent on accepting stationer intermediation. Later authors see mixed coordination-extraction (Tangled Rope)—the limited-term concept genuinely enables authorship as profession, but institutional gatekeeping extracts value. The knowledge commons sees pure extraction (Snare)—sealed concept, sealed institution, no exit. The 'limited right' narrative sees its own degradation over time (Piton)—the performance persists, institutional reality diverges. The open knowledge coalition sees a temporary lock with sunset (Scaffold)—alternative institutions (digital commons) can disentangle concept from monopoly. The analytical observer at civilizational scope verges on naturalizing entanglement (Mountain)—but the structural data (identifiable beneficiary, identifiable suppression mechanism) indicates false summit.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) reflects each agent's structural position relative to the entanglement. The Stationers' Company emerges as the practical beneficiary (low d → negative effective extraction experienced) despite nominal authorship focus (the institutional architecture ensures they capture value). Authors appear as nominal beneficiaries but are actually trapped victims (high d → high experienced extraction) because they cannot claim their rights without accepting stationer intermediation—the entanglement binds their identity to their oppressor. The knowledge commons has no exit and no identity option (d → 1.0, powerless scaling → high f(d)). Later authors who accept the authorship frame benefit from the concept (moderate d downward through beneficiary status) but pay the institutional cost (moderate d upward through victim status in distribution control). The conceptual clarity victim has no power atom and no perspective—the entanglement itself is the extraction. Directionality overrides are not needed because the structural derivation captures the essential asymmetry: the statute's genius is that it appears to benefit the least powerful agent (authors) while actually consolidating the most powerful agent's (stationers') monopoly.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conceptual_institutional_separability,
    'Could the Statute of Anne''s conceptual innovation (limited term, author focus) have been legislated WITHOUT simultaneously capturing institutional practice (Stationers'' monopoly), or are concept and institution genuinely inseparable?',
    'Counterfactual institutional history: what would a ''conceptual-only'' reform look like without institutional enforcement? Comparative analysis of later copyright reforms that attempted to separate concept from institution (e.g., US Copyright Office designs, open access mandates).',
    'If separable: the statute''s entanglement is a contingent design choice, not necessity — classification shifts from Tangled Rope toward pure Rope or institutional Snare depending on which dimension dominates. If inseparable: entanglement is structural feature of how conceptual innovation spreads institutionally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conceptual_institutional_separability, conceptual, 'Whether conceptual and institutional dimensions are logically separable').

omega_variable(
    nominal_vs_practical_beneficiary_divergence,
    'Did the Statute of Anne genuinely intend to benefit authors (as nominal rights-holders), or was authorship a rhetorical cover for consolidating Stationers'' monopoly with new legitimacy?',
    'Historical analysis of legislative intent (parliamentary records, petition histories, preface language). Empirical tracking of who actually captured rights and profits in first 20 years post-enactment. Comparison of author earnings/control across pre- and post-statute periods.',
    'If authorship was genuine intent: the snare classification is harsh; the constraint is a failed coordination mechanism (scaffold gone wrong). If authorship was rhetorical: the snare and rope classifications converge — the statue is pure institutional capture dressed in authorial language.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(nominal_vs_practical_beneficiary_divergence, empirical, 'Whether authorship benefit was genuine or rhetorical cover').

omega_variable(
    entanglement_as_methodological_problem,
    'Is the inability to disentangle concept and institution a feature of the 1710 statute itself, or a limitation of historical methodology—are we genuinely unable to separate them, or just unable to fully reconstruct what was ever separate?',
    'Archival recovery of pre-statute conceptual discussions (17th-century natural rights theories, continental precedents). Analysis of what intellectual resources were available to the statute''s architects. Determination of whether conceptual innovation preceded institutional lockdown or emerged alongside it.',
    'If methodological limit: the entanglement is an artifact of historical distance; concept and institution were separable to 1710 actors. If genuine structural feature: the entanglement reveals that institutional power and conceptual legitimacy co-constitute each other.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(entanglement_as_methodological_problem, empirical, 'Whether entanglement is structural or methodological artifact').

omega_variable(
    false_summit_naturalization_risk,
    'Does this reading naturalize the entanglement as inevitable (making the constraint appear as a mountain), when the entanglement is actually a contingent institutional design choice?',
    'Test against the false summit signature: if beneficiaries are identifiable (yes—Stationers'' Company), and the constraint naturalizes as ''concepts and institutions must co-constitute'' (yes), the mountain perspective is a false summit. Reframe the ''natural law'' as an extractive institutional design that benefits from appearing inevitable.',
    'If false summit confirmed: the mountain perspective should be reclassified or marked as showing how institutional power naturalizes contingency. If false summit rejected: the entanglement truly is a structural feature of how IP law works.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_naturalization_risk, conceptual, 'False summit risk: naturalizing contingent entanglement as law of nature').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statute_of_anne_ip_foundation__entangled_event_reading, 1710, 1760).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(statute_entangled_theater_1710, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 0, 0.52).
narrative_ontology:measurement(statute_entangled_theater_1735, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 25, 0.62).
narrative_ontology:measurement(statute_entangled_theater_1760, statute_of_anne_ip_foundation__entangled_event_reading, theater_ratio, 50, 0.68).

% Extraction over time
narrative_ontology:measurement(statute_entangled_extract_1710, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(statute_entangled_extract_1735, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 25, 0.54).
narrative_ontology:measurement(statute_entangled_extract_1760, statute_of_anne_ip_foundation__entangled_event_reading, base_extractiveness, 50, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(statute_entangled_suppress_1710, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(statute_entangled_suppress_1735, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 25, 0.6).
narrative_ontology:measurement(statute_entangled_suppress_1760, statute_of_anne_ip_foundation__entangled_event_reading, suppression_requirement, 50, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statute_of_anne_ip_foundation__entangled_event_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(statute_of_anne_ip_foundation__entangled_event_reading, 0.18).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__entangled_event_reading, statute_of_anne_ip_foundation__conceptual_emergence_reading).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__entangled_event_reading, statute_of_anne_ip_foundation__institutional_reallocation_reading).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__entangled_event_reading, copyright_term_extension_lifecycle).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__entangled_event_reading, author_publisher_intermediation_trap).

% DUAL FORMULATION NOTE:
% The statute of anne kernel decomposes into three constraint stories corresponding to three readings of the same historical event. Each reading yields a different constraint with a different epsilon: conceptual_emergence (ε=0.35, Rope focus) treats the innovation as coordination mechanism; institutional_reallocation (ε=0.55, Tangled Rope focus) treats the statute as forced reallocation with nominal benefits; entangled_event (ε=0.58, Tangled Rope focus) treats concept and institution as inseparable, revealing how structural entanglement enables extraction to masquerade as coordination. The three stories together model how historical events can be legitimately read through different structural lenses without reducing to 'all perspectives are equal'—different readings enable different diagnostic questions about institutional capture and conceptual naturalization.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(statute_of_anne_ip_foundation__entangled_event_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
