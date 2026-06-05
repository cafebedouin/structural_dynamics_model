% ============================================================================
% CONSTRAINT STORY: internet_archive_preservation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_internet_archive_preservation, []).

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
 *   constraint_id: internet_archive_preservation
 *   human_readable: The Internet Archive Preservation-Copyright Conflict
 *   domain: technological/legal
 *
 * SUMMARY:
 *   The Internet Archive (IA) acts as a global memory bank, but its
 *   operations are constrained by a high-stakes collision with copyright law.
 *   The IA scans and archives digital and physical content, making it
 *   available for public access. This directly conflicts with the interests
 *   of copyright holders, who argue that the IA's activities infringe on
 *   their intellectual property rights. The legal battles and negotiations
 *   surrounding this conflict shape the IA's ability to fulfill its mission.
 *
 * KEY AGENTS:
 *   - Copyright Holders: Primary victims (powerless/trapped) - experience extraction through unauthorized reproduction and distribution of their works.
 *   - Internet Archive: Tangled actor (moderate/constrained) - benefits from its mission but is constantly threatened by legal action.
 *   - Future Researchers: Primary beneficiaries (institutional/arbitrage) - gain access to preserved materials that might otherwise be lost.
 *   - Libraries and Educational Institutions: Organized actors (organized/mobile) - balance preservation with copyright concerns.
 *   - General Public: Beneficiary (moderate/constrained)- access to a wider range of knowledge
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(internet_archive_preservation, 0.55).
domain_priors:suppression_score(internet_archive_preservation, 0.6).
domain_priors:theater_ratio(internet_archive_preservation, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(internet_archive_preservation, extractiveness, 0.55).
narrative_ontology:constraint_metric(internet_archive_preservation, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(internet_archive_preservation, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(internet_archive_preservation, tangled_rope).
narrative_ontology:human_readable(internet_archive_preservation, "The Internet Archive Preservation-Copyright Conflict").
narrative_ontology:topic_domain(internet_archive_preservation, "technological/legal").

domain_priors:requires_active_enforcement(internet_archive_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(internet_archive_preservation, future_researchers).
narrative_ontology:constraint_beneficiary(internet_archive_preservation, general_public).
narrative_ontology:constraint_victim(internet_archive_preservation, copyright_holders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Copyright holders, particularly those with works in the long tail or orphan works, are significantly constrained by the IA's activities. The IA's scanning and distribution of copyrighted material, even for preservation purposes, directly impacts their potential revenue streams and control over their intellectual property. They are often trapped within the legal framework, lacking practical exit options due to the scale and reach of the IA's operations. Enforcement actions are costly and time-consuming.
constraint_indexing:constraint_classification(internet_archive_preservation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% The IA itself is in a tangled position. It benefits from its role as a digital library, attracting funding and public support. However, it's constrained by the constant threat of legal action from copyright holders. It has some mobility in negotiating licenses and advocating for fair use, but its core mission necessitates continued engagement in legally risky activities.
constraint_indexing:constraint_classification(internet_archive_preservation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% Future researchers are the primary beneficiaries. They gain access to preserved digital materials that might otherwise be lost. They have an 'arbitrage' exit because they will benefit from this resource, but do not bear the immediate risks of the archive itself. This is a civilizational-scale benefit.
constraint_indexing:constraint_classification(internet_archive_preservation, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% Libraries and educational institutions benefit from the IA's preservation efforts, which expand access to knowledge and resources. However, they are constrained by copyright law and the need to balance preservation with respecting intellectual property rights. The scaffold is that they can push for legislative changes and broader fair use doctrines, but this is a time-bound effort that may or may not succeed. If it does succeed, the 'scaffold' disappears.
constraint_indexing:constraint_classification(internet_archive_preservation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% From an analytical perspective, this is a tangled rope because the IA provides a valuable service (preservation) but does so in a way that extracts value from copyright holders. It is not a pure 'good' but a complex trade-off with winners and losers.
constraint_indexing:constraint_classification(internet_archive_preservation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(internet_archive_preservation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(internet_archive_preservation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(internet_archive_preservation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(internet_archive_preservation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(internet_archive_preservation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate-high. The IA's activities undeniably extract value from copyright holders by providing access to copyrighted works without explicit permission or compensation. The degree of extraction depends on legal interpretations of fair use and the availability of orphan works. Suppression (0.60): Moderate-high. The legal threats and restrictions imposed by copyright law significantly suppress the IA's ability to freely archive and distribute content. The IA must navigate a complex legal landscape and constantly defend its actions. Theater Ratio (0.30): Low. While some of the IA's activities may be performative (e.g., public advocacy for fair use), its core function of archiving and providing access to digital materials is substantive.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises from the conflicting interests of copyright holders and the IA's mission to preserve knowledge. Copyright holders perceive the IA as a threat to their economic interests, while the IA views itself as a defender of the public's right to access information. Future researchers and the general public are the clear beneficiaries of the IA's work, but their voices are often drowned out by the legal battles between the IA and copyright holders.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is determined by the structural relationship to the constraint. Copyright holders, as the primary victims, experience high extraction because they lose control over their intellectual property. The IA experiences moderate extraction because it faces legal challenges. Future researchers benefit, but do not actively extract or suppress. Libraries benefit, but are still constrained by their institutional ties.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fair_use_doctrine_scope,
    'What is the legally permissible scope of fair use for digital preservation?',
    'Court decisions in copyright infringement cases involving digital archives; legislative action clarifying fair use guidelines',
    'Narrow scope: IA faces increased legal challenges and may need to restrict access to materials. Broad scope: IA can operate with greater freedom, expanding its preservation efforts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fair_use_doctrine_scope, conceptual, 'Scope of fair use doctrine for digital preservation').

omega_variable(
    orphan_works_availability,
    'How readily available are orphan works through conventional channels?',
    'Surveys of orphan works accessibility in libraries, archives, and commercial databases; studies of the transaction costs associated with identifying and clearing rights for orphan works',
    'Low availability: IA provides a unique and valuable service by preserving and providing access to these works. High availability: IA''s activities may be viewed as less essential and more likely to infringe on copyright.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(orphan_works_availability, empirical, 'Availability of orphan works').

omega_variable(
    digital_rights_management_effectiveness,
    'To what extent can DRM technologies effectively protect copyrighted works while still allowing for preservation?',
    'Technical assessments of DRM systems'' ability to prevent unauthorized copying and distribution; studies of the impact of DRM on preservation efforts',
    'Effective DRM: Copyright holders may be more willing to allow preservation activities if their rights are adequately protected. Ineffective DRM: Copyright holders may remain resistant to preservation efforts, fearing unauthorized copying and distribution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(digital_rights_management_effectiveness, empirical, 'Effectiveness of DRM').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(internet_archive_preservation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inte_tr_t0, internet_archive_preservation, theater_ratio, 0, 0.1).
narrative_ontology:measurement(inte_tr_t5, internet_archive_preservation, theater_ratio, 5, 0.2).
narrative_ontology:measurement(inte_tr_t10, internet_archive_preservation, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(inte_be_t0, internet_archive_preservation, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(inte_be_t5, internet_archive_preservation, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(inte_be_t10, internet_archive_preservation, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(internet_archive_preservation, global_infrastructure).
narrative_ontology:affects_constraint(internet_archive_preservation, copyright_law).
narrative_ontology:affects_constraint(internet_archive_preservation, digital_rights_management).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
