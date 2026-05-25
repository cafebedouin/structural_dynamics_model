% ============================================================================
% CONSTRAINT STORY: knowledge_transmission_urgency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_knowledge_transmission_urgency, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: knowledge_transmission_urgency
 *   human_readable: Knowledge Transmission Urgency in Craft Labor Systems
 *   domain: political_economy/labor_systems/knowledge_transmission
 *
 * SUMMARY:
 *   The knowledge transmission urgency constraint captures the structural
 *   tension between embodied craft knowledge reproduction and industrial
 *   rationalization in late 19th/early 20th century factory systems. Lena's
 *   declining blow time (physiological constraint ceiling) creates a
 *   biological deadline for transmission. Friedrich's prohibition on teaching
 *   and Werner's delegation of only rough shaping work (allocation as
 *   extraction multiplier) create institutional suppression. The constraint
 *   exhibits high extractiveness (0.68) because the rationalized production
 *   system captures the output of embodied craft knowledge while
 *   systematically preventing its reproduction — a parasitic relationship
 *   that extracts from the knowledge commons without承擔 the cost of
 *   maintaining it. Theater ratio (0.42) reflects that some performative
 *   elements exist (formal training programs that claim to replace
 *   apprenticeship but cannot transmit tacit knowledge) but the constraint is
 *   primarily functional extraction rather than pure theater. The urgency
 *   dynamic is amplified by the intersection of two upstream constraints:
 *   physiological decline (mountain) sets the biological timeline;
 *   institutional prohibition (tangled rope) blocks alternative transmission
 *   pathways.
 *
 * KEY AGENTS:
 *   - Craft Knowledge Continuity: Primary victim (powerless/trapped) — abstract collective good facing extinction with no advocate; cannot exit the race between decline and prohibition
 *   - Aging Master Craftsperson (Lena): Primary victim (powerless/identity_locked) — identity constituted through embodied mastery; physiological decline creates urgency; institutional prohibition creates suppression
 *   - Apprentice Generation: Secondary victim (moderate/constrained) — excluded from transmission by institutional prohibition; those who gain access face precarious dependence on master defiance
 *   - Factory Ownership: Primary beneficiary (institutional/arbitrage) — captures output without承擔 reproduction cost; benefits from urgency-driven productivity and prohibition-driven cost reduction
 *   - Craft Guild Remnants: Organized victim (organized/constrained) — formal structures dismantled; informal networks persist but lack institutional power to enforce transmission
 *   - Industrial Education Reformers: Organized beneficiary (organized/mobile) — building alternative transmission pathways through technical schools and documented procedures; see urgency as temporary coordination problem with sunset
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the urgency as inherent to embodied skill systems rather than recognizing institutional amplification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(knowledge_transmission_urgency, 0.68).
domain_priors:suppression_score(knowledge_transmission_urgency, 0.75).
domain_priors:theater_ratio(knowledge_transmission_urgency, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(knowledge_transmission_urgency, extractiveness, 0.68).
narrative_ontology:constraint_metric(knowledge_transmission_urgency, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(knowledge_transmission_urgency, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(knowledge_transmission_urgency, snare).
narrative_ontology:human_readable(knowledge_transmission_urgency, "Knowledge Transmission Urgency in Craft Labor Systems").
narrative_ontology:topic_domain(knowledge_transmission_urgency, "political_economy/labor_systems/knowledge_transmission").

domain_priors:requires_active_enforcement(knowledge_transmission_urgency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(knowledge_transmission_urgency, factory_ownership).
narrative_ontology:constraint_beneficiary(knowledge_transmission_urgency, rationalized_production_system).
narrative_ontology:constraint_victim(knowledge_transmission_urgency, craft_knowledge_continuity).
narrative_ontology:constraint_victim(knowledge_transmission_urgency, embodied_skill_practitioners).
narrative_ontology:constraint_victim(knowledge_transmission_urgency, apprentice_generation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CRAFT KNOWLEDGE CONTINUITY (SNARE) — The embodied knowledge system itself cannot exit the race between physiological decline and institutional prohibition. Trapped by the biological timeline of aging practitioners and the economic logic of rationalized production. Maximum extraction: the knowledge commons faces extinction with no advocate and no alternative transmission pathway.
constraint_indexing:constraint_classification(knowledge_transmission_urgency, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: AGING MASTER CRAFTSPERSON (SNARE) — Identity-locked rather than structurally trapped: Lena's identity is constituted through the Dreh, the Gluthaut reading, the embodied mastery. She could theoretically exit (retire, move to another factory) but cannot abandon the knowledge without abandoning herself. Physiological decline creates urgency; institutional prohibition (Friedrich's ban on teaching, Werner's delegation of rough work only) creates suppression. High extraction: career culmination becomes a race against time with institutional barriers to transmission.
constraint_indexing:constraint_classification(knowledge_transmission_urgency, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 3: APPRENTICE GENERATION (TANGLED ROPE) — Constrained by the absence of formal apprenticeship structures and the prohibition on teaching, but also benefits when transmission does occur (access to high-value embodied skills that command wage premiums in remaining craft sectors). Mixed experience: genuine coordination function (skill transfer when it happens) alongside asymmetric extraction (most are excluded; those included face precarious access dependent on master's defiance of management).
constraint_indexing:constraint_classification(knowledge_transmission_urgency, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: FACTORY OWNERSHIP (ROPE) — Benefits from the urgency dynamic: aging masters work harder to transmit before decline, increasing short-term productivity; prohibition on formal apprenticeship reduces labor costs; rationalized production captures the output without承擔 the cost of knowledge reproduction. Experiences the constraint as coordination: managing the transition from craft to rationalized production. Net beneficiary with arbitrage exit options (can shift to different production models, geographies, or industries).
constraint_indexing:constraint_classification(knowledge_transmission_urgency, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: CRAFT GUILD REMNANTS (TANGLED ROPE) — Organized but constrained: formal guild structures have been dismantled or marginalized by industrial rationalization, but informal networks persist. Benefits from preserving craft knowledge (maintains guild identity and bargaining power in remaining craft sectors) but also bears extraction (institutional prohibition prevents formal transmission; physiological urgency outpaces organizational capacity to document and systematize embodied knowledge). Mixed coordination and extraction.
constraint_indexing:constraint_classification(knowledge_transmission_urgency, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: INDUSTRIAL EDUCATION REFORMERS (SCAFFOLD) — Organized agents building alternative transmission pathways: technical schools, documented procedures, rationalized training programs. See the urgency as a temporary coordination problem with a sunset: as embodied craft knowledge is codified into teachable procedures and institutional curricula, the dependence on aging masters declines. Low effective extraction because the coalition has agency and sees an exit path through systematization. Estimated sunset: 15-25 years for technical education infrastructure to replace apprenticeship model in core industrial sectors.
constraint_indexing:constraint_classification(knowledge_transmission_urgency, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational perspective, the race between physiological decline and knowledge transmission appears as an immutable feature of embodied skill systems: tacit knowledge always faces extinction risk when transmission depends on biological timelines. However, the structural data contradicts this naturalization — the urgency is amplified by institutional prohibition (Friedrich's ban, absence of formal apprenticeship), not by biology alone. The engine's false summit detector will reveal this as naturalization of a contingent political-economic arrangement.
constraint_indexing:constraint_classification(knowledge_transmission_urgency, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(knowledge_transmission_urgency_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(knowledge_transmission_urgency, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(knowledge_transmission_urgency, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(knowledge_transmission_urgency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(knowledge_transmission_urgency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The rationalized production system captures the output of embodied craft knowledge (Lena's mastery enables high-quality production) while systematically preventing its reproduction (Friedrich's prohibition, Werner's delegation of only rough work, absence of formal apprenticeship). This is parasitic extraction: the system depends on craft knowledge at critical nodes but refuses to bear the cost of maintaining the knowledge commons. The extraction increases over the interval as physiological decline accelerates urgency while institutional prohibition remains constant. Suppression (0.75): High. Multiple suppression mechanisms operate simultaneously: (1) physiological decline creates a biological deadline that cannot be extended; (2) institutional prohibition blocks formal transmission pathways; (3) absence of formal apprenticeship structures eliminates the traditional reproduction mechanism; (4) rationalized production ideology delegitimizes craft knowledge as obsolete, creating epistemic suppression. The suppression is structural rather than incidental — it is built into the political economy of industrial rationalization. Theater ratio (0.42): Moderate. Some performative elements exist: formal training programs claim to replace apprenticeship but cannot transmit tacit knowledge like the Dreh or Gluthaut reading; management rhetoric about 'modern methods' obscures continued dependence on embodied craft skills at critical production nodes. However, the constraint is primarily functional extraction (real knowledge loss, real production dependency) rather than pure theater. Theater increases over the interval as the gap between formal training claims and actual skill transmission widens.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon — the race between physiological decline and knowledge transmission — appears differently depending on the observer's position. The aging master craftsperson experiences it as a snare: identity-locked, facing biological deadline, blocked by institutional prohibition. The apprentice generation experiences it as tangled rope: genuine coordination function (skill transfer when it happens) alongside asymmetric extraction (most excluded, access precarious). Factory ownership experiences it as rope: coordination problem of managing the craft-to-rationalization transition while capturing output without reproduction cost. Industrial education reformers experience it as scaffold: temporary problem with a sunset as technical education infrastructure replaces apprenticeship. Craft guild remnants experience it as tangled rope: preserving knowledge maintains identity and bargaining power but organizational capacity is insufficient to overcome institutional prohibition. The analytical observer risks experiencing it as mountain: the urgency appears as an immutable feature of embodied skill systems, naturalizing what is actually institutional amplification of a biological constraint. The perspectival gap reveals that the 'urgency' is not purely biological — it is a product of institutional prohibition intersecting with physiological decline.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality values reflect each agent's structural position relative to the extraction flow. Craft knowledge continuity and aging master craftspersons are victims with trapped/identity_locked exit options — they bear maximum extraction as the knowledge commons faces extinction and individual practitioners race against physiological decline with institutional barriers to transmission. The apprentice generation is a victim with constrained exit options — excluded from transmission but not entirely without agency (some gain access through master defiance; skills remain valuable in remaining craft sectors). Factory ownership is a beneficiary with arbitrage exit options — captures output without reproduction cost and can shift to alternative production models if craft knowledge becomes unavailable. Craft guild remnants are victims with constrained exit options — organizational capacity exists but institutional power has been dismantled. Industrial education reformers are beneficiaries with mobile exit options — building alternative pathways and can shift strategies if technical education proves insufficient. The analytical observer uses the analytical exit option and risks naturalizing the urgency as inherent to embodied skill systems rather than recognizing institutional amplification.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that high extractiveness (0.68) does not imply zero coordination function. The knowledge transmission system, when it operates, provides genuine coordination: masters transmit valuable embodied skills that enable high-quality production and command wage premiums. The extraction arises not from the transmission mechanism itself but from the institutional prohibition that prevents transmission while continuing to depend on the knowledge. This is the tangled rope pattern at the system level: coordination function exists (skill transfer) but is embedded within an extractive structure (prohibition on reproduction while capturing output). The snare classification from the powerless perspectives (craft knowledge continuity, aging master) reflects that these agents cannot exit the extraction — they are trapped by biology and institutional barriers. The rope classification from the institutional beneficiary perspective (factory ownership) reflects that this agent experiences net benefit and has exit options. The scaffold classification from the organized reformer perspective reflects that alternative transmission pathways are being built with a sunset logic. No single type is 'the' answer — the presheaf over observation sites captures the full structural reality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    codification_feasibility,
    'What proportion of embodied craft knowledge (Dreh, Gluthaut reading) can be codified into teachable procedures vs. requiring direct master-apprentice transmission?',
    'Comparative analysis of skill acquisition outcomes: apprentices trained via documented procedures vs. direct embodied transmission; measurement of performance gaps in complex tasks requiring tacit knowledge',
    'If high codifiability: scaffold perspective confirmed — technical education can replace apprenticeship. If low codifiability: snare perspective confirmed — knowledge extinction is structural without direct transmission pathways.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(codification_feasibility, empirical, 'Feasibility of codifying embodied craft knowledge').

omega_variable(
    prohibition_enforcement_variance,
    'How consistently is the institutional prohibition on teaching enforced across factories, regions, and time periods?',
    'Historical analysis of factory records, labor disputes, and apprenticeship survival rates; identification of enforcement gaps and defiance patterns',
    'If enforcement is inconsistent: suppression is lower than measured, and some transmission pathways persist through institutional gaps. If enforcement is consistent: suppression is structural, and transmission depends entirely on master defiance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(prohibition_enforcement_variance, empirical, 'Variance in enforcement of teaching prohibition').

omega_variable(
    physiological_decline_timeline,
    'What is the typical timeline between peak embodied skill mastery and physiological decline that prevents effective transmission?',
    'Longitudinal tracking of master craftsperson performance metrics (blow time, defect rates, endurance) correlated with age; identification of transmission window duration',
    'If window is long (15+ years): urgency is lower, and transmission has structural slack. If window is short (5-10 years): urgency is high, and institutional prohibition creates acute extinction risk.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(physiological_decline_timeline, empirical, 'Duration of effective transmission window before decline').

omega_variable(
    rationalization_substitution_completeness,
    'Can rationalized production fully substitute for craft knowledge, or does it depend on residual embodied skills at critical nodes?',
    'Analysis of production quality and failure modes in fully rationalized vs. hybrid craft-rationalized systems; identification of irreducible craft dependencies',
    'If substitution is complete: factory ownership perspective confirmed — craft knowledge is obsolete. If substitution is incomplete: rationalized system depends parasitically on craft knowledge it refuses to reproduce, revealing hidden extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rationalization_substitution_completeness, empirical, 'Completeness of rationalization''s substitution for craft knowledge').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(knowledge_transmission_urgency, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kt_theater_early, knowledge_transmission_urgency, theater_ratio, 0, 0.25).
narrative_ontology:measurement(kt_theater_mid, knowledge_transmission_urgency, theater_ratio, 5, 0.35).
narrative_ontology:measurement(kt_theater_late, knowledge_transmission_urgency, theater_ratio, 10, 0.42).

% Extraction over time
narrative_ontology:measurement(kt_extract_early, knowledge_transmission_urgency, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(kt_extract_mid, knowledge_transmission_urgency, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(kt_extract_late, knowledge_transmission_urgency, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(knowledge_transmission_urgency, identity_coordination).
narrative_ontology:boltzmann_floor_override(knowledge_transmission_urgency, 0.12).

% DUAL FORMULATION NOTE:
% This constraint is downstream of two structurally distinct upstream constraints: physiological_constraint_ceiling (mountain — biological decline timeline) and allocation_as_extraction_multiplier (tangled rope — institutional prohibition and work delegation patterns). The knowledge transmission urgency is the intersection of these: biology sets the deadline; institutional prohibition blocks alternative pathways. The extractiveness of this constraint (0.68) is distinct from and higher than the upstream constraints because it captures the parasitic relationship between rationalized production and craft knowledge reproduction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(knowledge_transmission_urgency, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
