% ============================================================================
% CONSTRAINT STORY: maat_order_principle__distributed_maintenance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maat_order_principle__distributed_maintenance_reading, []).

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
 *   constraint_id: maat_order_principle__distributed_maintenance_reading
 *   human_readable: Ma'at Order Principle: Distributed Maintenance Reading
 *   domain: ancient_egyptian_religion/political_philosophy/distributed_governance
 *
 * SUMMARY:
 *   The Ma'at order principle in ancient Egypt represents a contested kernel
 *   with multiple legitimate readings. This story instantiates the
 *   distributed maintenance reading: the claim that Ma'at (cosmic order,
 *   justice, balance) is maintained collectively through proper conduct at
 *   all social levels. In this reading, a Pharaoh maintains Ma'at through
 *   ritual and governance; a priest maintains it through cult and judgment; a
 *   laborer maintains it through honest work and household order. All actors
 *   are legitimate Ma'at-keepers, and all actors face accountability if they
 *   fail — authority derives from demonstrated stewardship rather than
 *   inherent status. This reading coexists with two sibling interpretations:
 *   the divine mandate reading (Ma'at flows from the gods through the Pharaoh
 *   downward) and the reciprocity reading (Ma'at exchange between social
 *   strata creates obligation chains). The distributed reading treats the
 *   constraint as a genuine rope — a coordination mechanism with minimal
 *   extraction because accountability flows through all levels and no single
 *   actor can extract indefinitely without losing legitimacy. The low
 *   extractiveness (0.18) and low suppression (0.25) reflect that this
 *   reading posits transparent accountability rather than coercive hierarchy.
 *   The theater ratio (0.35) is moderate because ritual performance is
 *   central to Ma'at maintenance, but the ritual is functional (it
 *   demonstrates commitment to order) rather than performative (it covers up
 *   extraction).
 *
 * KEY AGENTS:
 *   - Pharaoh: Institutional keeper of Ma'at (institutional/arbitrage) — benefits from order but authority depends on demonstrated maintenance
 *   - Priesthood: Organized interpreter of Ma'at (organized/constrained) — authority flows from stewardship and ritual competence
 *   - Provincial Governors and Local Officials: Distributed agents (powerful/constrained) — responsible for maintaining Ma'at in their domains; legitimacy is conditional
 *   - Craftspeople, Guilds, and Community Leadership: Organized groups (organized/constrained) — coordinate local order through their own stewardship
 *   - Commoners and Laborers: Distributed agents (powerless/constrained) — maintain Ma'at through honest work and proper conduct in their station; face accountability through social integration
 *   - The Cosmos/Ma'at itself: The abstract collective good (powerless/trapped) — victim if any agent fails to maintain it, but also beneficiary of distributed responsibility
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maat_order_principle__distributed_maintenance_reading, 0.18).
domain_priors:suppression_score(maat_order_principle__distributed_maintenance_reading, 0.25).
domain_priors:theater_ratio(maat_order_principle__distributed_maintenance_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maat_order_principle__distributed_maintenance_reading, rope).
narrative_ontology:human_readable(maat_order_principle__distributed_maintenance_reading, "Ma'at Order Principle: Distributed Maintenance Reading").
narrative_ontology:topic_domain(maat_order_principle__distributed_maintenance_reading, "ancient_egyptian_religion/political_philosophy/distributed_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(maat_order_principle__distributed_maintenance_reading, 'maat-reading-dist-001').
narrative_ontology:cs_kernel_codification('maat-reading-dist-001', fixed_text).
narrative_ontology:cs_authority_grounding('maat-reading-dist-001', practice).
narrative_ontology:cs_interpretation_layer_present('maat-reading-dist-001').
narrative_ontology:cs_reading_relation('maat-reading-dist-001', maat_order_principle__divine_mandate_reading, coexists_with).
narrative_ontology:cs_reading_relation('maat-reading-dist-001', maat_order_principle__reciprocity_reading, influences).
narrative_ontology:cs_axiom('maat-reading-dist-001', foundational, authority_grounded_in_demonstrated_stewardship).
narrative_ontology:cs_axiom_status(authority_grounded_in_demonstrated_stewardship, holdable).
narrative_ontology:cs_axiom_grounding('maat-reading-dist-001', authority_grounded_in_demonstrated_stewardship, conventional).
narrative_ontology:cs_axiom('maat-reading-dist-001', foundational, all_social_strata_legitimate_maat_keepers).
narrative_ontology:cs_axiom_status(all_social_strata_legitimate_maat_keepers, overridden).
narrative_ontology:cs_axiom_grounding('maat-reading-dist-001', all_social_strata_legitimate_maat_keepers, deontological).
narrative_ontology:cs_reference_frame('maat-reading-dist-001', collective_stewardship_legitimacy).
narrative_ontology:cs_drift_state('maat-reading-dist-001', late_ptolemaic_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('maat-reading-dist-001', '').
narrative_ontology:cs_kernel_id(maat_order_principle__distributed_maintenance_reading, maat_order_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, all_social_strata).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, cosmic_order_itself).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COMMONER (ROPE) — The common laborer, craftsperson, or farmer experiences Ma'at maintenance as genuine coordination responsibility: proper conduct in one's station (honest work, just dealings, maintaining household order) sustains the shared cosmic order. No extraction — the constraint binds all equally. Exit is constrained by social integration, not by coercive mechanism. The commoner's perspective sees the constraint as fundamentally fair: I do my part, you do yours, and the cosmos stays balanced.
constraint_indexing:constraint_classification(maat_order_principle__distributed_maintenance_reading, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 2: ORGANIZED ACTORS (ROPE) — Scribal guilds, priesthoods, craftspeople guilds, and community leadership see Ma'at maintenance as a coordination mechanism that solves collective action problems: everyone benefits when order is maintained, everyone suffers when it degrades. The guild's authority derives from demonstrated stewardship, not inherent status. Extraction is minimal because accountability flows upward — fail to maintain Ma'at and lose legitimacy. This is the reading's core: authority is earned through maintenance, not granted by birth.
constraint_indexing:constraint_classification(maat_order_principle__distributed_maintenance_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PHARAOH (ROPE) — The Pharaoh experiences the constraint differently than an absolute monarch would. In this reading, pharaonic authority is NOT unconditional extraction — it is conditional on demonstrated Ma'at maintenance. The Pharaoh's role as Ma'at-keeper requires constant ritual performance, judgment-making, and stewardship. Failure to maintain Ma'at threatens the Pharaoh's legitimacy and power. The Pharaoh has arbitrage options (can potentially shift the reading to divine mandate or reciprocity framings) but does so at cost — deviation from distributed-maintenance framing risks legitimacy collapse. The Pharaoh benefits from order, but the constraint constrains rather than enables pure extraction.
constraint_indexing:constraint_classification(maat_order_principle__distributed_maintenance_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PRIESTHOOD (ROPE) — The priestly class experiences Ma'at maintenance as a coordination function that legitimizes their authority: priests maintain Ma'at through ritual, judgment, and institutional knowledge. But the coordination is real — if priests fail to maintain proper ritual and order, cosmic chaos threatens everyone. The priesthood cannot extract indefinitely without consequences. Suppression is low (priests cannot compel belief through force alone); extraction is low (priests benefit from coordinating others' conduct toward shared order). The priesthood's power derives from demonstrable stewardship, not from monopoly over coercion.
constraint_indexing:constraint_classification(maat_order_principle__distributed_maintenance_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PROVINCIAL GOVERNOR (ROPE) — Regional authorities experience Ma'at as both enabling and constraining: they can organize provincial order efficiently (genuine coordination benefit), but they cannot extract indefinitely without losing legitimacy. The constraint creates accountability pressure — a governor's authority depends on maintaining visible Ma'at in their province. If the region falls into visible disorder, the governor's position is threatened. Suppression is moderate (the governor has coercive capacity but cannot rely on it alone); extraction is low (the governor benefits from order, but the coordination is not purely exploitative).
constraint_indexing:constraint_classification(maat_order_principle__distributed_maintenance_reading, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (ROPE) — From civilizational perspective, this reading models a distributed responsibility system where legitimacy flows from demonstrated stewardship. The constraint solves coordination problems (how to maintain order across a complex society) without requiring top-down coercion or belief in divine mandates. It is a genuine rope: all actors benefit from order, all actors contribute to maintaining it, and all actors face accountability if they fail. Suppression is low because the mechanism is internalized (identity-based, not coercive). Theater is low because the constraint's function is transparent: Ma'at maintenance IS the point, not a cover for extraction.
constraint_indexing:constraint_classification(maat_order_principle__distributed_maintenance_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maat_order_principle__distributed_maintenance_reading_tests).
:- end_tests(maat_order_principle__distributed_maintenance_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. The distributed maintenance reading posits that authority derives from demonstrated stewardship rather than coercive power. No single agent can extract indefinitely — if the Pharaoh fails to maintain Ma'at, legitimacy erodes; if priests fail, they lose authority; if commoners fail, social integration suffers. The constraint is fundamentally coordinative: all actors benefit from order, and all bear costs if order fails. The low value reflects that the mechanism is not extractive but coordinative. The slight upward drift over 1000 years (0.12 → 0.22) reflects that as the system stabilized, some performative overhead and hierarchical sedimentation likely accumulated — but even at T1000, the reading characterizes the constraint as fundamentally distributive. Suppression (0.25): Low-moderate. The distributed model does not rely on heavy coercion — it relies on internalized responsibility and social integration. Suppression is present (actors cannot simply ignore Ma'at requirements without consequences), but not through force alone. Theater ratio (0.35): Low-moderate. Ritual performance is central to Ma'at maintenance (processions, offerings, judgment scenes), but the performance is functional — it demonstrates commitment to order, not covers up extraction. The reading treats ritual as transparent, not performative.
 *
 * PERSPECTIVAL GAP:
 *   The distributed maintenance reading produces Rope classification across all perspectives because the constraint is fundamentally coordinative at all social levels. The commoner sees coordination (we all maintain order together); the community sees coordination (stewardship duties bind us); the Pharaoh sees coordination constrained by legitimacy (I benefit from order, but I cannot extract indefinitely); the priesthood sees coordination (our authority flows from stewardship); provincial governors see conditional authority (I can organize efficiently, but failure costs legitimacy); the analytical observer sees a transparent rope mechanism. No perspective produces Snare or Tangled Rope because the reading eliminates the asymmetric extraction that those types require. The perspectival gap is not between different types (all are Rope) but between this reading and sibling readings: the divine mandate reading would produce institutional beneficiary at high extraction; the reciprocity reading would produce different vulnerability patterns. Within the distributed maintenance reading, the gap is between ideal (all accountability is equal) and practice (some actors have more capacity to evade accountability).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from the structural relationship of each agent to the constraint. Commoners are beneficiaries of order and constrainers of order simultaneously — beneficiary status (d low) is offset by their trapped exit options (d rises), producing mid-range d ≈ 0.45-0.50, which yields moderate but not maximal chi. Pharaohs are beneficiaries (d low from arbitrage options) but also targets (high accountability pressure), producing mixed d ≈ 0.35-0.40. Priests are beneficiaries (authority flows from stewardship) but accountable (loss of function = loss of authority), producing d ≈ 0.35-0.45. The distributed reading's key structural feature is that no agent's directionality is extreme — all agents occupy mixed positions in the beneficiary-victim spectrum. This produces the Rope type across all perspectives because effective extraction (chi) remains in the coordinative range (≤ 0.35) even at high spatial scope. Contrast this with the divine mandate reading, where d would be more extreme (Pharaoh d ≈ 0.05, commoner d ≈ 0.90), producing higher chi and Snare classification at powerless level.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    distributed_vs_hierarchical_authority,
    'Does the ''distributed maintenance'' reading actually describe how authority worked in practice, or does it idealize a hierarchical system (divine mandate reading) into a more egalitarian discourse?',
    'Textual analysis of Egyptian sources (Instruction texts, tomb biographies, administrative records) comparing frequency and explicitness of distributed accountability language vs. divine mandate language across time periods and social strata; correlation with archaeological evidence of actual power distribution and enforcement mechanisms',
    'If distributed model is descriptively accurate: the reading captures a genuine structural feature and ε = 0.18 stands. If it is aspirational idealization by subordinate strata: actual extractiveness is higher (ε ≈ 0.35-0.45), and the constraint is Tangled Rope, not Rope. The reading itself would be overridden by evidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributed_vs_hierarchical_authority, empirical, 'Whether distributed maintenance reading describes actual authority structure or idealizes hierarchical system').

omega_variable(
    legitimacy_accountability_mechanism,
    'What concrete mechanisms enforce accountability for Ma''at maintenance? Can an actor actually lose authority by failing to maintain Ma''at, or is the threat of legitimacy loss merely rhetorical while de facto power remains coercive?',
    'Historical cases of pharaonic succession disputes, priestly purges, and provincial rebellions; analysis of whether failed Ma''at maintenance (documented in records) actually triggered loss of office or merely triggered propaganda reframing; identification of actors who lost power specifically for failing Ma''at duties vs. those removed by pure coercion',
    'If mechanisms are real and functional: accountability is genuine, suppression is low, Rope classification stands. If accountability is rhetorical (de facto coercion persists regardless of Ma''at maintenance): suppression is high, constraint is Tangled Rope or Snare, reading is partially overridden.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_accountability_mechanism, empirical, 'Existence and functionality of accountability mechanisms for Ma''at maintenance').

omega_variable(
    commoner_agency_in_distributed_model,
    'To what extent did commoners actually have agency in maintaining or withdrawing support for Ma''at order, vs. experiencing the ''distributed maintenance'' framing as ideological cover for imposed hierarchy?',
    'Analysis of strike records (Deir el-Medina), grain tax resistance, migration patterns, and peasant rebellions; linguistic analysis of who uses ''distributed maintenance'' framing in texts (elites vs. non-elites); comparison of maintenance language in official vs. private documents',
    'If commoner agency is real: distributed rope model holds. If commoners are passive subjects told they are maintaining order: constraint is Tangled Rope (mixed coordination and extraction), theater is higher, suppression is higher. The reading would be partially identity-locked — commoners internalize distributed responsibility framing while lacking actual exit options.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(commoner_agency_in_distributed_model, empirical, 'Actual agency of commoners in maintaining or withdrawing support for Ma''at order').

omega_variable(
    reading_kernel_stability,
    'This reading instantiates a specific interpretation of the Ma''at kernel: that distributed maintenance is the legitimacy criterion. But does the Egyptian textual record actually support this reading, or is it a modern analytical projection?',
    'Systematic textual corpus analysis: frequency and contexts of maintenance language (seshemu, snty, etc.) across genres; comparison with divine mandate language (neter, khent); identification of which social strata explicitly articulate distributed responsibility in their own voice vs. which receive it as imposed interpretation',
    'If reading is well-grounded in source material: constraints story is authentic. If reading is modern projection: the story describes an analytical interpretation rather than an Egyptian understanding. This becomes a conceptual omega about historical interpretation methodology, not about Ma''at itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_stability, conceptual, 'Whether distributed maintenance reading is grounded in Egyptian sources or a modern analytical projection').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maat_order_principle__distributed_maintenance_reading, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maat_dist_theater_t0, maat_order_principle__distributed_maintenance_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(maat_dist_theater_t500, maat_order_principle__distributed_maintenance_reading, theater_ratio, 500, 0.32).
narrative_ontology:measurement(maat_dist_theater_t1000, maat_order_principle__distributed_maintenance_reading, theater_ratio, 1000, 0.35).

% Extraction over time
narrative_ontology:measurement(maat_dist_extract_t0, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(maat_dist_extract_t500, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 500, 0.18).
narrative_ontology:measurement(maat_dist_extract_t1000, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 1000, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(maat_dist_suppress_t0, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(maat_dist_suppress_t500, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 500, 0.24).
narrative_ontology:measurement(maat_dist_suppress_t1000, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 1000, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maat_order_principle__distributed_maintenance_reading, attachment_coordination).
narrative_ontology:affects_constraint(maat_order_principle__distributed_maintenance_reading, maat_order_principle__divine_mandate_reading).
narrative_ontology:affects_constraint(maat_order_principle__distributed_maintenance_reading, maat_order_principle__reciprocity_reading).

% DUAL FORMULATION NOTE:
% The Ma'at order principle is a single kernel with three structural readings. This story addresses the distributed maintenance reading only. The divine mandate reading (extractiveness ≈ 0.48, Tangled Rope from pharaonic perspective, Snare from commoner perspective) instantiates a different power structure where Ma'at flows downward from gods through Pharaoh, enabling higher extraction. The reciprocity reading (extractiveness ≈ 0.32, Tangled Rope across most perspectives) treats Ma'at as obligations flowing between strata, creating mutual vulnerability. All three are linked via network.affects_constraints and represent different ways Egyptian sources interpret the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
