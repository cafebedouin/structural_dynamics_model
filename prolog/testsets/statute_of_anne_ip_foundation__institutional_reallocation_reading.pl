% ============================================================================
% CONSTRAINT STORY: statute_of_anne_ip_foundation__institutional_reallocation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statute_of_anne_ip_foundation__institutional_reallocation_reading, []).

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
 *   constraint_id: statute_of_anne_ip_foundation__institutional_reallocation_reading
 *   human_readable: Statute of Anne: Institutional Reallocation Reading—IP Rights Transfer from Stationers to Authors
 *   domain: legal_history/intellectual_property/institutional_economics
 *
 * SUMMARY:
 *   The Statute of Anne (1710) formally reallocated the right to control book
 *   printing and distribution. Under the Stationers' Company regime, the
 *   Company held a monopoly on printing licensed books; the statute
 *   transferred this authority to individual authors, who could then assign
 *   rights to publishers. This reading treats the statute as primarily an
 *   institutional reallocation—the occupied institutional position changed
 *   hands from the Company to a new class (authors, with practical benefit
 *   flowing to publishers via assignment). This is one of three structurally
 *   distinct readings of the statute. The institutional reallocation reading
 *   focuses on WHO HOLDS THE RIGHT and how the occupying group changed; it
 *   does NOT claim that a new conceptual category of 'copyright' was invented
 *   (that is the conceptual_emergence reading); it does NOT claim that
 *   institutional and conceptual change were simultaneous and inseparable
 *   (that is the entangled_event reading). This reading's analytical frame is
 *   institutional economics: property rights systems are sets of occupying
 *   roles, beneficiaries are those who can extract value from occupying or
 *   assigning those roles, and change consists of reallocation of who
 *   occupies the institutional space.
 *
 * KEY AGENTS:
 *   - Stationers' Company: Pre-statute monopolist (institutional/arbitrage) — primary victim of formal reallocation; retains economic control through infrastructure for decades post-statute
 *   - Authors: New statutory rights-holders (moderate/constrained) — formal beneficiaries but constrained by inability to enforce rights or negotiate favorable assignment terms
 *   - Publishers (established firms): Primary beneficiaries via assignment (institutional/arbitrage) — gain legitimacy to control publication and can extract through assignment contracts
 *   - Journeyman printers: Trapped labor in guild structure (powerless/trapped) — victims of both old monopoly and new regime; neither benefit from statute
 *   - Organized author professions (18th-19th centuries): Coalition building collective leverage (organized/mobile) — see statute as temporary scaffold enabling transition to author-negotiated terms
 *   - Analytical observer: Civilizational view (analytical/analytical) — risks naturalizing institutional reallocation as discovering immutable natural right to authorial property
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.52).
domain_priors:suppression_score(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.58).
domain_priors:theater_ratio(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statute_of_anne_ip_foundation__institutional_reallocation_reading, tangled_rope).
narrative_ontology:human_readable(statute_of_anne_ip_foundation__institutional_reallocation_reading, "Statute of Anne: Institutional Reallocation Reading—IP Rights Transfer from Stationers to Authors").
narrative_ontology:topic_domain(statute_of_anne_ip_foundation__institutional_reallocation_reading, "legal_history/intellectual_property/institutional_economics").

domain_priors:requires_active_enforcement(statute_of_anne_ip_foundation__institutional_reallocation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statute_of_anne_ip_foundation__institutional_reallocation_reading, '0a470aa4-1683-4880-9acb-eb3599513a13').
narrative_ontology:cs_kernel_codification('0a470aa4-1683-4880-9acb-eb3599513a13', formalized).
narrative_ontology:cs_authority_grounding('0a470aa4-1683-4880-9acb-eb3599513a13', lineage).
narrative_ontology:cs_interpretation_layer_present('0a470aa4-1683-4880-9acb-eb3599513a13').
narrative_ontology:cs_reading_relation('0a470aa4-1683-4880-9acb-eb3599513a13', statute_of_anne_ip_foundation__conceptual_emergence_reading, coexists_with).
narrative_ontology:cs_reading_relation('0a470aa4-1683-4880-9acb-eb3599513a13', statute_of_anne_ip_foundation__entangled_event_reading, influences).
narrative_ontology:cs_axiom('0a470aa4-1683-4880-9acb-eb3599513a13', foundational, institutional_position_primacy).
narrative_ontology:cs_axiom_status(institutional_position_primacy, holdable).
narrative_ontology:cs_axiom_grounding('0a470aa4-1683-4880-9acb-eb3599513a13', institutional_position_primacy, instrumental).
narrative_ontology:cs_axiom('0a470aa4-1683-4880-9acb-eb3599513a13', foundational, existing_rights_reallocated).
narrative_ontology:cs_axiom_status(existing_rights_reallocated, holdable).
narrative_ontology:cs_axiom_grounding('0a470aa4-1683-4880-9acb-eb3599513a13', existing_rights_reallocated, empirically_contingent).
narrative_ontology:cs_reference_frame('0a470aa4-1683-4880-9acb-eb3599513a13', stationers_company_monopoly_regime).
narrative_ontology:cs_drift_state('0a470aa4-1683-4880-9acb-eb3599513a13', contemporary_publisher_dominance, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('0a470aa4-1683-4880-9acb-eb3599513a13', '').
narrative_ontology:cs_kernel_id(statute_of_anne_ip_foundation__institutional_reallocation_reading, statute_of_anne_ip_foundation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__institutional_reallocation_reading, book_publishers_via_assignment).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__institutional_reallocation_reading, authors_with_assignment_capacity).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__institutional_reallocation_reading, stationers_company_monopoly).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__institutional_reallocation_reading, unassigned_authors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: JOURNEYMAN PRINTER (SNARE) — Caught between Stationers' Company enforcement and the new statute's author-centric regime. Cannot exit the guild structure (career trapped), cannot benefit from the statute (no authorship standing). Bears suppression from both the old monopoly and the new regime without exit.
constraint_indexing:constraint_classification(statute_of_anne_ip_foundation__institutional_reallocation_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INDEPENDENT/MARGINAL AUTHOR (TANGLED ROPE) — Gains notional rights under the statute but constrained by lack of market power to enforce them or negotiate assignment terms. The statute coordinates publication (enables entry) but extracts through assignment coercion: publishers demand perpetual or quasi-perpetual assignments as condition of printing. Constrained exit (cannot negotiate favorable terms without losing publication opportunity).
constraint_indexing:constraint_classification(statute_of_anne_ip_foundation__institutional_reallocation_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ESTABLISHED PUBLISHER (ROPE) — Net beneficiary. The statute creates coordination mechanism: authors hold rights, publishers acquire them through assignment. Publishers with capital and distribution networks can arbitrage between author-held rights and commercial value. Experience is coordination (enabling publication pipelines) with embedded asymmetric extraction (assignment terms heavily favor publishers). Effective exit option: can arbitrage across multiple authors; no individual author necessary.
constraint_indexing:constraint_classification(statute_of_anne_ip_foundation__institutional_reallocation_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: AUTHOR ASSOCIATION / ORGANIZED PROFESSION (SCAFFOLD) — Sees the statute as a temporary scaffold supporting the transition from monopoly to individualized rights. In the medium term (generational), authors organized collectively have leverage to renegotiate assignment terms (via professional societies, copyright reform movements). Mobile exit (can coordinate with peers, can advocate for reform). Theater relatively low: the statutory mechanism is genuinely functional, not performative.
constraint_indexing:constraint_classification(statute_of_anne_ip_foundation__institutional_reallocation_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: STATIONERS' COMPANY (PITON) — The old monopoly persists through institutional inertia despite the statute's formal reallocation. Stationers still control printing infrastructure and market access; they still enforce guild membership and apprenticeship rules; the statute's formal transfer of rights is substantially performative because the underlying economic power (capital, distribution, guild enforcement) has not shifted. The company decays over centuries but maintains leverage through control of the printing apparatus itself.
constraint_indexing:constraint_classification(statute_of_anne_ip_foundation__institutional_reallocation_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational view, the statute appears to instantiate a natural and unchanging principle: intellectual labor naturally produces a right held by the laborer. This perspective risks naturalizing the statute as discovering an immutable principle rather than constructing a novel institutional arrangement. The base properties will reveal this as a false summit: the 'natural' holder of IP rights is a contingent institutional choice, not a law of nature.
constraint_indexing:constraint_classification(statute_of_anne_ip_foundation__institutional_reallocation_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statute_of_anne_ip_foundation__institutional_reallocation_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(statute_of_anne_ip_foundation__institutional_reallocation_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(statute_of_anne_ip_foundation__institutional_reallocation_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(statute_of_anne_ip_foundation__institutional_reallocation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(statute_of_anne_ip_foundation__institutional_reallocation_reading, TR),
    TR >= 0.70.

:- end_tests(statute_of_anne_ip_foundation__institutional_reallocation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The statute creates a new institutional position for authors to occupy, but the practical benefit flows primarily to publishers via assignment. The extraction rises over time (0.35 → 0.58) as publishers develop standardized assignment contracts and market power consolidates. By the 50-year mark, the constraint has evolved from a relatively straightforward reallocation (authors hold rights, can assign them) into a mechanism for extracting labor surplus—authors receive payments but publishers control commercial terms. Suppression (0.58): Moderate-high and rising. Initially (t0=0.42), the suppression is the Stationers' Company's residual power and authors' lack of enforcement capacity. Over time (t25=0.55, t50=0.58), suppression increases because publishers' collective power to demand assignment becomes more standardized and coercive. Authors can theoretically refuse assignment but face exclusion from the publishing market. Theater ratio (0.35): Low-moderate, indicating genuine coordination function rather than pure performance. The statute is not merely performative—it actually changes who holds the right and enables new publication arrangements. The theater is not 'review ritual' (as in verification bottleneck) but rather the maintenance of the formal legal claim that authors hold rights while the practical economic benefit accrues to publishers.
 *
 * PERSPECTIVAL GAP:
 *   This constraint manifests distinctly across perspectives. Established publishers experience it as Rope—a coordination mechanism enabling publication pipelines with favorable extraction terms via assignment. Independent authors experience it as Tangled Rope—genuine coordination (can now publish through assignment) with embedded extraction (lose commercial control). The Stationers' Company experiences it as Piton—formally displaced but economically persistent through control of printing infrastructure. Journeyman printers experience it as Snare—trapped between guild enforcement and the new regime. The organized author profession sees it as Scaffold—a temporary institutional arrangement enabling transition toward collective bargaining leverage. The analytical observer risks seeing Mountain—naturalizing the author-as-rightsholder as discovering an immutable principle rather than recognizing it as institutional choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation for this constraint tracks the flow of economic benefit and institutional authority. Publishers (beneficiary + arbitrage) derive low d → low effective extraction from their perspective; they have exit capacity and capture the structural advantage. Authors (beneficiary on paper, victim in practice + constrained) have moderate d: they formally hold rights but cannot exercise them without publisher intermediation. The Stationers' Company (victim + arbitrage) experiences high d from their perspective—they lose formal authority despite retaining practical economic control. Journeyman printers (victim + trapped) experience maximum d—no exit option, no benefit. The engine's directionality derivation chain computes d from these structural positions and applies the sigmoid f(d) to yield effective chi. Perspectives with high d show higher experienced extractiveness; perspectives with low d show coordination dominance.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves mandatrophy by reframing it as a perspectival diversity phenomenon. The constraint is NOT purely extractive (snare) because genuine coordination function exists—the statute enables publication that would not occur under pure monopoly. But neither is it pure coordination (rope) because extraction is structurally embedded in assignment contracts. The tangled_rope classification holds across most perspectives. The extraction does NOT disqualify coordination, and the coordination does NOT eliminate extraction. The mandatrophy is resolved by accepting that tangled_rope is the appropriate classification for a constraint that genuinely does both, with perspectival variation (some actors see rope, some see snare) that reveals the structure underneath.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reallocation_vs_creation_ambiguity,
    'Does the statute reallocate pre-existing rights from the Stationers'' Company to authors, or does it create a new category of rights that did not previously exist?',
    'Textual analysis of the statute''s language (does it use ''reallocate,'' ''transfer,'' ''grant anew,'' or ''recognize''); comparison with the Company''s claimed rights under the old regime; examination of whether pre-statute author claims are treated as property disputes or novel assertions.',
    'If reallocation: the institutional reading holds — the occupied set changed but the rights category existed. If creation: the statute is a conceptual emergence reading — it creates a new category of rights that had no institutional precedent. This choice determines whether the constraint is primarily about redistribution (tangled_rope) or invention (creative emergence).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reallocation_vs_creation_ambiguity, empirical, 'Whether the statute reallocates existing rights or creates new ones').

omega_variable(
    assignment_coercion_vs_negotiation,
    'To what extent are author-publisher assignments voluntary negotiations versus coerced capitulation to publishers'' market power?',
    'Historical analysis of assignment contract terms, author bargaining capacity relative to publishers, frequency of authors retaining rights vs assigning them, documentation of disputes or objections to assignment practices, comparison with periods of greater author leverage (post-professional organization).',
    'If highly coercive: the tangled_rope extraction is substantial and the suppression gates (suppression ≥ 0.40) are robust. If substantially voluntary: the constraint may be better classified as rope (coordination with embedded benefit for publishers rather than extraction). Current assessment assumes moderate coercion (suppression 0.58) based on structural power imbalance in early 18th-century book trade.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(assignment_coercion_vs_negotiation, empirical, 'Whether author-publisher assignments are voluntary or coerced').

omega_variable(
    institutional_change_temporal_scope,
    'Over what timescale does the institutional reallocation ''happen''? Is it immediate legal shift (statute signed) or gradual behavioral transition as actors adapt to new rules?',
    'Longitudinal analysis of litigation records, guild records, and publishing contracts pre- and post-statute; documentation of when authors first successfully claimed rights against Stationers'' Company; measurement of how long the old monopoly''s economic control persists despite legal reallocation.',
    'If immediate: the institutional reading is clean — rights shifted at enactment. If gradual: the reallocation is institutional theater (piton reading) — formal legal change but economic power persists, occupying the institutional space through inertia rather than formal authority. Affects measurements: faster transition reduces theater_ratio; slower transition increases it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_change_temporal_scope, empirical, 'Timescale of institutional reallocation from legal change to behavioral shift').

omega_variable(
    kernel_reading_distinctness,
    'This reading (institutional reallocation) claims the statute''s primary action is redistributing an existing institutional position from the Stationers'' Company to a new class (authors/publishers). Does this reading coherently distinguish itself from the sibling ''entangled_event'' reading, which holds that institutional and conceptual change cannot be separated?',
    'Axiom articulation: this reading''s foundational claim is that the institutional change (who holds the right) is primary and conceptually detachable from any new conceptual categories introduced. The entangled reading denies this separability. The readings coexist if authors (and historical analysis) genuinely treat institutional reallocation and conceptual emergence as distinct phenomena—i.e., if some actors see ''IP rights moving from Company to authors'' while others see ''a fundamentally new right being created.'' If all actors universally experience them as fused, coexists_with is still valid (different theoretical frameworks, same underlying phenomenon).',
    'If readings are genuinely distinct in actors'' lived experience: coexists_with is correct. If one reading is merely an analytical disaggregation of what actors experience as unified: the relationships may need revision toward forecloses or influences. Current assessment: coexists_with is correct—institutional reallocation is how company lawyers and authors experience the change; conceptual emergence is how natural-rights philosophers frame it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_distinctness, conceptual, 'Whether institutional reallocation reading coherently distinguishes itself from entangled_event reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soa_inst_reall_theater_t0, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(soa_inst_reall_theater_t25, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 25, 0.32).
narrative_ontology:measurement(soa_inst_reall_theater_t50, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 50, 0.35).

% Extraction over time
narrative_ontology:measurement(soa_inst_reall_extractiveness_t0, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(soa_inst_reall_extractiveness_t25, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 25, 0.52).
narrative_ontology:measurement(soa_inst_reall_extractiveness_t50, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 50, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(soa_inst_reall_suppression_t0, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(soa_inst_reall_suppression_t25, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 25, 0.55).
narrative_ontology:measurement(soa_inst_reall_suppression_t50, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statute_of_anne_ip_foundation__institutional_reallocation_reading, resource_allocation).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__institutional_reallocation_reading, statute_of_anne_ip_foundation__conceptual_emergence_reading).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__institutional_reallocation_reading, statute_of_anne_ip_foundation__entangled_event_reading).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__institutional_reallocation_reading, publisher_author_assignment_extraction).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__institutional_reallocation_reading, perpetual_copyright_extension_debates).

% DUAL FORMULATION NOTE:
% The Statute of Anne kernel generates three sibling constraint stories with different ε values. The institutional reallocation reading (ε=0.52) models the statute as redistributing an existing institutional position. The conceptual emergence reading (separate story) models the statute as creating a fundamentally new regulatory concept. The entangled event reading (separate story) models them as inseparable. These are not three observations of the same constraint—they are three distinct constraints that share a kernel (the historical statute) but instantiate different analytical frames. The network links identify the family structure. Network edges to downstream constraints (publisher_author_assignment_extraction, perpetual copyright extension debates) recognize that the institutional reallocation reading directly influences downstream constraints through the assignment mechanism it creates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(statute_of_anne_ip_foundation__institutional_reallocation_reading, institutional, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
