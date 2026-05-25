% ============================================================================
% CONSTRAINT STORY: beneficiary_agency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_beneficiary_agency_reading, []).

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
 *   constraint_id: beneficiary_agency_reading
 *   human_readable: Reformer-Printer Coalition Authority Bypass (Beneficiary Agency Reading)
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This constraint models the Reformation-era deployment of printing
 *   technology as a deliberate strategy by reformer networks and printing
 *   entrepreneurs to bypass Church institutional authority over doctrinal
 *   distribution. Rather than treating printing as an exogenous technological
 *   force that 'caused' the Reformation (technological determinism), this
 *   reading emphasizes that reformers and printers were intentional
 *   beneficiaries who strategically directed printing capacity toward
 *   religious content as a mechanism to capture authority-setting power from
 *   the Church. The constraint is hybrid: it exhibits genuine coordination
 *   function (solving the problem of rapid, scalable doctrinal distribution)
 *   and asymmetric extraction (the coalition captures authority previously
 *   monopolized by Church). Both groups benefit from the
 *   arrangement—reformers gain theological influence without requiring
 *   hierarchical Church approval; printers access a new, lucrative market
 *   segment. The Church and scribal production bear extraction costs. The
 *   theater_ratio declines over the measurement interval (0.62 → 0.35) as
 *   printing transforms from novelty (requiring theatrical justification) to
 *   infrastructure (requiring no justification), while extractiveness
 *   increases (0.18 → 0.52) as the scale and scope of authority capture
 *   expands. This reading instantiates one specific framing of a contested
 *   kernel: technology_reformation_causality. The alternative readings
 *   (technological_determinism_reading, co_constitution_reading) model the
 *   same historical phenomena as different constraints with different ε
 *   values and different beneficiary/victim structures. The
 *   beneficiary_agency_reading isolates the strategic choice component and
 *   attributes causal force to the reformer-printer coalition's deliberate
 *   deployment of available technology.
 *
 * KEY AGENTS:
 *   - Reformer Networks (Luther, Zwingli, Calvin, their epistolary and organizational networks): Primary beneficiary (organized/mobile) — gain theological influence, doctrinal authority, and distributed messaging coordination. Deliberately direct printing toward religious content.
 *   - Printing Entrepreneurs (Gutenberg, Froben, Koberger, merchant printer-publishers): Primary beneficiary (organized/arbitrage) — access new market for religious texts; profit from scale; relocate presses to advantage. Strategically align with high-demand reformist content.
 *   - Scribal Production System (monastic scriptoria, professional copyists, manuscript guilds): Primary victim (powerless/trapped) — lose income, status, and purpose as printing volume redirects copying work. Trapped within guild structures with no alternative livelihood.
 *   - Church Institutional Authority (Pope, bishops, doctrinal councils, central administration): Victim (powerful/trapped) — loses monopoly on doctrinal legitimation and reproduction control. Cannot exit institutional structure; suppression mechanisms (burning, censorship) become increasingly expensive and ineffective.
 *   - Local Religious Communities and Merchants: Secondary victim/beneficiary (moderate/constrained) — experience mixed extraction (censorship restricts content) and coordination (cheaper books enable wider access).
 *   - Church Licensing/Regulatory Response (Index, imprimatur, printing privileges): Counter-institutional actor (institutional/constrained) — attempted absorption of the constraint through regulation; experiences mixed coordination (licensing allocates legitimacy) and extraction (fees, content control).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beneficiary_agency_reading, 0.52).
domain_priors:suppression_score(beneficiary_agency_reading, 0.48).
domain_priors:theater_ratio(beneficiary_agency_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beneficiary_agency_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(beneficiary_agency_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(beneficiary_agency_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beneficiary_agency_reading, tangled_rope).
narrative_ontology:human_readable(beneficiary_agency_reading, "Reformer-Printer Coalition Authority Bypass (Beneficiary Agency Reading)").
narrative_ontology:topic_domain(beneficiary_agency_reading, "history_of_technology/religious_history/media_studies").

domain_priors:requires_active_enforcement(beneficiary_agency_reading).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beneficiary_agency_reading, reformer_networks).
narrative_ontology:constraint_beneficiary(beneficiary_agency_reading, printing_entrepreneurs).
narrative_ontology:constraint_victim(beneficiary_agency_reading, church_institutional_authority).
narrative_ontology:constraint_victim(beneficiary_agency_reading, manuscript_scribal_production).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SCRIBAL PRODUCTION SYSTEM (SNARE) — Trapped within Church-regulated manuscript reproduction. Unable to exit or compete; bears full extraction cost as printing volume redirects copying work and income. No alternative technology available during transition. Suppression enforced through guild structures and Church monopoly on legitimation.
constraint_indexing:constraint_classification(beneficiary_agency_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: LOCAL COMMUNITIES & MERCHANTS (TANGLED ROPE) — Constrained by Church doctrinal control and distance from printing centers, but also benefit from cheaper, faster access to religious texts and commercial prints. Experience mixed extraction and coordination: the constraint provides both opportunity (cheaper books) and barrier (restricted content through licensing/censorship).
constraint_indexing:constraint_classification(beneficiary_agency_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: REFORMER NETWORKS (ROPE) — Organized agents (Luther, Zwingli, their epistolary networks) experience the constraint as enabling coordination. Printing becomes the infrastructure for distributed theology — pamphlets, biblical translations, and polemical broadsheets coordinate reformist messaging across regions. The constraint appears as a solution to the collective-action problem of doctrinal dissemination. Low effective extraction because reformers have agency and mobility.
constraint_indexing:constraint_classification(beneficiary_agency_reading, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 4: PRINTING ENTREPRENEURS (ROPE) — Organized agents (Gutenberg, Froben, Koberger) see printing technology and reformer demand as a coordination mechanism for market expansion. The constraint is experienced as beneficial: reformist texts are commercially lucrative, and reformers need their production and distribution. Arbitrage access (can relocate presses, switch content types) means low experienced extraction — the entrepreneurs are beneficiaries.
constraint_indexing:constraint_classification(beneficiary_agency_reading, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: CHURCH INSTITUTIONAL AUTHORITY (SNARE) — Trapped within the authority structure it created. Cannot exit the monopoly on doctrinal legitimation without ceasing to be the Church. Experiences the constraint as pure extraction: reformers and printers capture authority-setting power through parallel distribution. The Church's suppression mechanisms (burning, censorship, ex post facto licensing) become increasingly expensive and ineffective as printing scale grows. Maximum experienced extraction for this powerful actor reveals the constraint's true asymmetry.
constraint_indexing:constraint_classification(beneficiary_agency_reading, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 6: CHURCH REGULATORY RESPONSE (TANGLED ROPE) — Counter-institutional actor (Index librorum prohibitorum, printing privileges, imprimatur systems) attempting to absorb the constraint through regulation. Experiences mixed extraction and coordination: licensing coordinate access to legitimacy while extracting fees and controlling content. Constrained because the regulatory response is reactive, not constitutive — printers and reformers already have alternative legitimacy channels.
constraint_indexing:constraint_classification(beneficiary_agency_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / BENEFICIARY AGENCY FRAME (TANGLED ROPE) — From the vantage of universal scope, the reformer-printer coalition deliberately deployed printing as a tool to bypass Church authority. Both groups benefited from the arrangement (theological influence for reformers, market expansion for printers), and both actively directed technology toward this goal. The constraint is hybrid: genuine coordination mechanism (solving the problem of document distribution at scale) and asymmetric extraction from Church and scribal production. Technology is not the cause; beneficiary agency is. The analytical frame reveals that technology's 'impact' is retroactive meaning-making — observers see inevitability in choices that were actually strategic decisions by motivated agents.
constraint_indexing:constraint_classification(beneficiary_agency_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(beneficiary_agency_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(beneficiary_agency_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(beneficiary_agency_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(beneficiary_agency_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(beneficiary_agency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The reformer-printer coalition captures significant authority-setting power from the Church through parallel distribution channels. The extraction is not total (the Church retains institutional authority, only its monopoly erodes) and is contingent on continued printing demand. The value reflects measurable authority transfer without complete institutional collapse. The extraction increases over the measurement interval from 0.18 to 0.52 as printing scale grows and authority shift solidifies. Suppression (0.48): Moderate. The coalition faces real barriers—Church licensing restrictions, burning of heretical texts, censorship of reformist content, threat of excommunication for printers—but these suppression mechanisms prove insufficient to block printing entirely. Reformers and printers have enough resources and geographic mobility (ability to relocate presses to friendlier jurisdictions) to maintain production despite suppression. Theater ratio (0.35): Low-to-moderate, declining. Early printing (t=0) required theatrical justification as a novel technology with uncertain legitimacy (0.62 theater). By t=50, printing is accepted infrastructure with minimal theatrical content (0.35). The decline reflects that as technology becomes embedded, the performative element recedes and the functional (either coordination or extraction) becomes visible.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap emerges from the contrast between beneficiary and victim experiences. The reformer networks and printers experience the constraint as rope (coordination for their mutual benefit) because they have agency, mobile access, and shared interest in printing's scaling. The Church experiences the constraint as snare (trapped extraction) because it cannot exit its monopoly claim while printers and reformers establish parallel authority. The scribal production system experiences snare (trapped, no alternative). Local communities experience tangled rope (mixed barriers and benefits). The analytical observer (beneficiary_agency frame) sees tangled rope from the system level: genuine coordination function (document distribution at scale) plus asymmetric extraction (authority capture). The perspectival gap reveals that the same structural phenomenon—printing's scale advantage—appears as beneficial coordination to those who control it and as extractive authority loss to those who cannot. This gap would collapse under alternative readings: technological_determinism would show printing as external force affecting all perspectives similarly; co_constitution would dissolve single-perspective classifications into network effects.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective derives from the agent's structural relationship to the authority-bypass extraction. Reformers with mobile/organized power and arbitrage exit options experience low d (0.20–0.30)—they are beneficiaries of the constraint. Printers with arbitrage mobility similarly experience low d (0.15–0.25). The Church, despite its powerful position globally, experiences high d (0.90–0.95) relative to THIS constraint because it cannot exercise its global power against the distributed printing network—the constraint strips its specific authority. Scribal production experiences maximum d (0.98)—trapped, powerless, with no exit. The derivation chain (beneficiary/victim + exit_options → d → f(d)) produces the perspectival differences: beneficiaries with exit options get low chi (coordination); victims with trapped exit get high chi (extraction). The analytical observer's d (0.72, canonical for analytical position) produces the tangled_rope classification.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the Reformation-printing relationship best understood as reformer-printer beneficiary agency orchestrating technology toward doctrinal bypass (beneficiary_agency_reading), as technological determinism where printing caused Reformation (technological_determinism_reading), or as co-constitution where technology and theology mutually shaped each other (co_constitution_reading)?',
    'Historical primary sources analysis: epistolary evidence of deliberate strategy by reformers and printers; counterfactual analysis of Reformation trajectories without printing; analysis of non-Reformation uses of printing (commercial, state administration) to isolate the technology from the theological outcome.',
    'If beneficiary_agency: constraint is tangled_rope from analytical perspective; reformers and printers are knowingly extracting from Church authority. If technological_determinism: constraint appears as scaffold (printing as inevitable force) or piton (degraded theological authority due to technological disruption). If co_constitution: constraint dissolves into network of interdependent constraints with no single causality direction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which reading of technology-Reformation causality is structurally correct').

omega_variable(
    sibling_reading_technological_determinism,
    'Under the technological_determinism_reading (alternative constraint story), would printing be classified as scaffold (temporary disruptor) or piton (accelerating degradation of Church authority through institutional inertia)?',
    'Comparison of printing diffusion timelines with Church regulatory response timelines; analysis of whether Church institutional failure preceded or followed printing adoption.',
    'Technological_determinism_reading would show printing as exogenous shock, making the Church''s authority loss appear inevitable rather than strategically undermined by beneficiary agency. Changes the perspectival gap: powerful actors (Church) would classify as victims of technological inevitability rather than victims of deliberate extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_technological_determinism, conceptual, 'Technological determinism reading would classify printing differently').

omega_variable(
    sibling_reading_co_constitution,
    'Under the co_constitution_reading (alternative constraint story), would the reformer-printer coalition and Church regulatory response be modeled as separate constraints in a network rather than as a single tangled_rope?',
    'Decomposition analysis: are the coalition''s coordination function and the Church''s suppression response structurally distinct constraints (network decomposition) or aspects of a single hybrid constraint? If decomposed, what are the separate ε values and how do they interact?',
    'Co_constitution_reading would likely generate 2-3 linked constraints rather than one tangled_rope, revealing interdependencies and feedback loops. This reading would make the constraint family approach central rather than treating single-constraint perspectivalism as primary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_co_constitution, conceptual, 'Co-constitution reading would decompose into constraint network').

omega_variable(
    beneficiary_deliberateness_evidence,
    'What level of documentary evidence suffices to establish that reformers and printers deliberately strategized the use of printing for authority bypass versus opportunistically exploiting a technological advantage?',
    'Primary source analysis: explicit coordination evidence (letters, contracts, theological justifications for printing strategy); comparison with non-Reformation uses of printing in the same period to isolate deliberate religious choice from technological inevitability.',
    'If only opportunistic: constraint shifts toward snare (extraction happens but is not orchestrated) or rope (coordination of a tool for shared benefit without strategic authority bypass). If deliberate and orchestrated: constraint solidifies as tangled_rope with high beneficiary agency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_deliberateness_evidence, empirical, 'Degree of documented reformer-printer strategic coordination').

omega_variable(
    technology_causality_epistemic_trap,
    'Does framing technology as ''tool not cause'' (beneficiary_agency_reading) risk invisiblizing structural technological effects and over-attributing causal power to human intentionality?',
    'Comparison of Reformation outcomes in regions with early vs late printing adoption; analysis of whether reformer messages spread equally effectively through manuscript, preaching, and printing networks; counterfactual: would Reformation have occurred at similar scale and speed without printing?',
    'If technology effects are substantial: beneficiary_agency_reading underweights the constraint''s technological component and should incorporate co-constitution elements. If agency is primary: current framing is appropriate and alternative readings are overcorrections against earlier technological determinism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_causality_epistemic_trap, conceptual, 'Risk of overcorrecting from technological determinism into pure agency attribution').

omega_variable(
    temporal_scope_permissibility,
    'Is the beneficiary_agency_reading''s temporal scope (biographical, generational for institutional actors) appropriate, or should the constraint''s effects be traced through civilizational scope to capture the long-term authority erosion that printing enabled?',
    'Measurement of institutional Church authority metrics across centuries: doctrinal compliance, revenue control, cultural influence before and after printing adoption; assessment of whether the constraint''s extraction persists or reaches a terminal point.',
    'If civilizational scope is more appropriate: constraint may be better classified as piton (degraded authority persisting through inertia) from Church perspective at longer timescales. Current biographical framing may underweight the durability of the authority capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporal_scope_permissibility, empirical, 'Appropriate temporal scope for the constraint''s effects').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beneficiary_agency_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(benef_agency_theater_t0, beneficiary_agency_reading, theater_ratio, 0, 0.62).
narrative_ontology:measurement(benef_agency_theater_t25, beneficiary_agency_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement(benef_agency_theater_t50, beneficiary_agency_reading, theater_ratio, 50, 0.35).

% Extraction over time
narrative_ontology:measurement(benef_agency_extract_t0, beneficiary_agency_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(benef_agency_extract_t25, beneficiary_agency_reading, base_extractiveness, 25, 0.4).
narrative_ontology:measurement(benef_agency_extract_t50, beneficiary_agency_reading, base_extractiveness, 50, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(beneficiary_agency_reading, information_standard).
narrative_ontology:affects_constraint(beneficiary_agency_reading, technological_determinism_reading).
narrative_ontology:affects_constraint(beneficiary_agency_reading, co_constitution_reading).

% DUAL FORMULATION NOTE:
% The technology_reformation_causality kernel decomposes into three constraint stories representing different readings of the same historical phenomenon. beneficiary_agency_reading emphasizes intentional actor choice and strategic deployment (ε=0.52, tangled_rope from analytical perspective). technological_determinism_reading would model printing as exogenous technological shock (higher ε, scaffold or piton from most perspectives). co_constitution_reading would decompose into multiple linked constraints showing mutual causality and feedback loops. Each reading is complete and ε-invariant within itself; the kernel contest is routed through omega variables in each story. The network links enable the engine to trace which reading is selected and how classification would change under alternatives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(beneficiary_agency_reading, institutional, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
