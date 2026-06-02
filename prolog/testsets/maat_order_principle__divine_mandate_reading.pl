% ============================================================================
% CONSTRAINT STORY: maat_order_principle__divine_mandate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maat_order_principle__divine_mandate_reading, []).

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
 *   constraint_id: maat_order_principle__divine_mandate_reading
 *   human_readable: Ma'at Divine Mandate: Pharaoh as Cosmic Order Source
 *   domain: ancient_history/political_philosophy/religious_studies
 *
 * SUMMARY:
 *   The divine mandate reading of Ma'at constructs a unidirectional cosmic
 *   order flowing from primordial divine principles through the Pharaoh as
 *   their terrestrial embodiment and down to subject society. In this
 *   framework, the Pharaoh does not serve Ma'at — the Pharaoh IS Ma'at's
 *   instrument and cannot violate it by definition, because violation would
 *   be logically impossible; the ruler's actions define Ma'at by being the
 *   Pharaoh's actions. This reading concentrates extraction authority in the
 *   pharaonic office and suppresses alternative readings (particularly the
 *   reciprocity reading, which asserts the Pharaoh is bound by Ma'at
 *   constraints, and the distributed maintenance reading, which treats Ma'at
 *   as a collective responsibility). The constraint exhibits a characteristic
 *   false summit signature: it presents as a natural law (cosmic necessity)
 *   but is actually an institutional arrangement that maximizes extraction by
 *   the pharaonic-priestly coalition. The suppression shows a rising
 *   trajectory (0.68→0.82) as the institutional apparatus for enforcing the
 *   divine mandate reading matured — textual standardization, priestly
 *   education, execration practices, and damnatio memoriae all served to
 *   close off alternative interpretations. Theater ratio also rises
 *   (0.48→0.65) as ritual performance became increasingly central to
 *   demonstrating the Pharaoh's Ma'at-channeling function, and as the
 *   connection between ritual performance and actual cosmic maintenance
 *   became more conceptually distant.
 *
 * KEY AGENTS:
 *   - Pharaonic Authority: Primary beneficiary (institutional/constrained) — sole mediator of cosmic order; gains extraction legitimacy; paradoxically trapped by the theological framework that justifies extraction
 *   - Temple Priesthood: Secondary beneficiary (institutional/arbitrage) — gains access to resources and authority through ritual role as Pharaoh's intermediaries; can arbitrage between interpretations but choose divine mandate reading
 *   - Subject Population: Primary victim (powerless/trapped) — bears taxation, labor obligation, and resource requisition framed as cosmic maintenance; epistemic closure prevents exit
 *   - Alternative Cosmological Readings: Structural victim (none/analytical) — suppressed as heretical or treasonous; the divine mandate reading crowds out reciprocity and distributed maintenance frameworks
 *   - Analytical Observer: Civilization view (analytical/analytical) — risks naturalizing institutional extraction as cosmic law; can recognize false summit through network decomposition with sibling readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maat_order_principle__divine_mandate_reading, 0.68).
domain_priors:suppression_score(maat_order_principle__divine_mandate_reading, 0.82).
domain_priors:theater_ratio(maat_order_principle__divine_mandate_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maat_order_principle__divine_mandate_reading, snare).
narrative_ontology:human_readable(maat_order_principle__divine_mandate_reading, "Ma'at Divine Mandate: Pharaoh as Cosmic Order Source").
narrative_ontology:topic_domain(maat_order_principle__divine_mandate_reading, "ancient_history/political_philosophy/religious_studies").

domain_priors:requires_active_enforcement(maat_order_principle__divine_mandate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(maat_order_principle__divine_mandate_reading, 'uuid-kernel-maat-divine-mandate-2026-02-26').
narrative_ontology:cs_kernel_codification('uuid-kernel-maat-divine-mandate-2026-02-26', fixed_text).
narrative_ontology:cs_authority_grounding('uuid-kernel-maat-divine-mandate-2026-02-26', extraction).
narrative_ontology:cs_interpretation_layer_present('uuid-kernel-maat-divine-mandate-2026-02-26').
narrative_ontology:cs_reading_relation('uuid-kernel-maat-divine-mandate-2026-02-26', maat_order_principle__reciprocity_reading, forecloses).
narrative_ontology:cs_reading_relation('uuid-kernel-maat-divine-mandate-2026-02-26', maat_order_principle__distributed_maintenance_reading, coexists_with).
narrative_ontology:cs_axiom('uuid-kernel-maat-divine-mandate-2026-02-26', foundational, pharaoh_cosmic_sole_mediator).
narrative_ontology:cs_axiom_status(pharaoh_cosmic_sole_mediator, holdable).
narrative_ontology:cs_axiom_grounding('uuid-kernel-maat-divine-mandate-2026-02-26', pharaoh_cosmic_sole_mediator, theological).
narrative_ontology:cs_axiom('uuid-kernel-maat-divine-mandate-2026-02-26', foundational, pharaonic_action_defines_maat).
narrative_ontology:cs_axiom_status(pharaonic_action_defines_maat, holdable).
narrative_ontology:cs_axiom_grounding('uuid-kernel-maat-divine-mandate-2026-02-26', pharaonic_action_defines_maat, theological).
narrative_ontology:cs_reference_frame('uuid-kernel-maat-divine-mandate-2026-02-26', cosmic_order_through_pharaonic_mediation).
narrative_ontology:cs_drift_state('uuid-kernel-maat-divine-mandate-2026-02-26', late_new_kingdom, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('uuid-kernel-maat-divine-mandate-2026-02-26', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(maat_order_principle__divine_mandate_reading, maat_order_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maat_order_principle__divine_mandate_reading, pharaonic_authority).
narrative_ontology:constraint_beneficiary(maat_order_principle__divine_mandate_reading, temple_priesthood).
narrative_ontology:constraint_victim(maat_order_principle__divine_mandate_reading, subject_population).
narrative_ontology:constraint_victim(maat_order_principle__divine_mandate_reading, alternative_cosmological_readings).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBJECT POPULATION (SNARE) — Trapped within the theological framework that legitimizes royal authority as cosmic necessity. Cannot exit the constraint or question the Pharaoh's embodiment of Ma'at without existential transgression. Bears the full cost of royal extraction (taxation, labor, resource requisition) justified as maintenance of cosmic order. Zero degrees of freedom for questioning the framework itself.
constraint_indexing:constraint_classification(maat_order_principle__divine_mandate_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: TEMPLE PRIESTHOOD (ROPE) — Benefits substantially from the divine mandate reading. Priests are positioned as the Pharaoh's ritual intermediaries and gain access to land, resources, and authority by maintaining the Ma'at ideology. Experience the constraint as coordination of cosmic maintenance. Can arbitrage between different theological interpretations but choose not to, as the current reading maximizes their institutional position.
constraint_indexing:constraint_classification(maat_order_principle__divine_mandate_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 3: PHARAONIC AUTHORITY (SNARE) — This reading paradoxically places the Pharaoh outside and above the constraint system: the ruler is the SOURCE of Ma'at, not its subject. Yet the perspective classifies as snare because the Pharaoh becomes trapped by the theological commitment that justifies extraction. Any admission that the ruler can violate Ma'at destabilizes the entire legitimacy structure. The Pharaoh is suppressed from revising the framework even while appearing to be its origin.
constraint_indexing:constraint_classification(maat_order_principle__divine_mandate_reading, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER — NATURAL LAW VIEW (MOUNTAIN) — This reading presents Ma'at as an immutable cosmic order that the Pharaoh merely channels rather than creates. From a civilizational view, the constraint appears as a law of nature (the cosmos requires a mediating figure for maintenance of order). However, the structural data reveals this as a false summit: the 'cosmic necessity' framing naturalizes what is actually an institutional arrangement that concentrates extraction authority in the pharaonic office.
constraint_indexing:constraint_classification(maat_order_principle__divine_mandate_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maat_order_principle__divine_mandate_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(maat_order_principle__divine_mandate_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(maat_order_principle__divine_mandate_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(maat_order_principle__divine_mandate_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(maat_order_principle__divine_mandate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The divine mandate reading justifies substantial pharaonic extraction (taxation at 10-20% of productivity, corvée labor for monument construction, control of trade and temple resources) by claiming cosmic necessity. The Pharaoh's position as sole mediator of Ma'at creates an extraction monopoly — no alternative suppliers of cosmic maintenance exist. The value reflects the reading's institutional dominance and the difficulty subjects face in challenging the legitimacy frame. Suppression (0.82): Very high. The constraint suppresses alternative readings through institutional mechanisms: priestly education standardization, scribal training that replicates divine mandate ideology, execration texts that eliminate names of those who questioned pharaonic Ma'at identity, damnatio memoriae practiced against heretical kings, and theological closure that makes the Pharaoh's Ma'at identity axiomatic rather than arguable. Theater ratio (0.65): Moderate-high. While the divine mandate reading is functionally important (organizing temple ritual, legitimizing labor mobilization, providing cosmological meaning), a rising proportion of pharaonic activity becomes performative: ritual recitations asserting Ma'at maintenance that have decreasing demonstrable connection to actual agricultural or administrative outcomes. As the reading matured, the theatrical component increased — more resources devoted to reasserting the Pharaoh's divine nature through monuments and inscriptions, fewer resources devoted to adaptive governance.
 *
 * PERSPECTIVAL GAP:
 *   The divine mandate reading creates a unique perspectival structure. The Pharaoh appears as the source of the constraint (standing outside it), yet classifies as snare because the ruler becomes trapped by the theological commitment that justifies extraction. Any admission that the Pharaoh can violate Ma'at destabilizes the entire extraction legitimacy. The priesthood sees rope — genuine coordination of cosmic maintenance through ritual. The subject population sees snare — pure extraction with no escape. The analytical observer sees a false summit: the 'cosmic necessity' framing naturalizes what is actually an institutional power concentration. The perspectival gap reveals that the 'divine mandate' is most legible as extraction to those bearing its costs, while appearing as coordination and cosmic duty to beneficiaries.
 *
 * DIRECTIONALITY LOGIC:
 *   The pharaonic authority occupies a paradoxical structural position in this reading. Nominally, the Pharaoh is the source of Ma'at and stands outside the constraint system. However, the constraint's structure binds the ruler through the very theological framework that justifies extraction: the Pharaoh cannot admit fallibility or constraint without unraveling the legitimacy apparatus. The priesthood, as institutional beneficiaries with arbitrage options (able to promote alternative cosmological readings but choosing not to), experience low directionality — the constraint works in their favor. Subject populations, trapped within the theological framework with no exit, experience maximal directionality toward victimhood. The false summit detector will identify this reading as problematic: a 'natural law' constraint (cosmic necessity) with identifiable beneficiaries (pharaonic-priestly coalition) and mechanisms of suppression (institutional enforcement of the divine mandate reading). The contradiction between emerges_naturally: false and the mountain-like framing in the analytical perspective signals false summitry.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading creates mandatrophy at two levels. First, the analytical observer reading it as mountain (cosmic law) versus snare (institutional extraction) represents the core mandatrophy: is Ma'at an immutable principle or a constructed justification? The reading's own logic prevents resolution — any evidence of pharaonic constraint becomes reframed as cosmic necessity (the Pharaoh cannot violate Ma'at, so observed constraints must reflect divine will). Second, the reading forecloses alternatives: if the Pharaoh is the sole source of Ma'at, the reciprocity reading (Pharaoh bound by Ma'at) and distributed maintenance reading (collective responsibility) become logically impossible within the same theological framework. Yet historically, all three readings coexisted across different time periods and institutional factions. The mandatrophy resolves by recognizing that this constraint story instantiates only ONE reading of a genuinely ambiguous kernel. The sibling constraints (divine_mandate_reading's counterparts in the reciprocity and distributed maintenance readings) provide the resolution: Ma'at itself remains underdetermined, and the divine mandate reading is one institutional choice among others, not the only logically coherent position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_mandate_vs_reciprocal_obligation,
    'Is Ma''at a unidirectional mandate flowing from cosmos through Pharaoh downward (divine mandate reading), or a reciprocal obligation binding both ruler and cosmos (reciprocity reading)?',
    'Textual analysis of royal inscriptions, pyramid texts, and wisdom literature: frequency of passages claiming pharaonic agency vs. passages asserting pharaonic constraint; analysis of failure narratives (crop failure, rebellion) as divine punishment vs. human error; examination of ritual formulas asserting royal duty vs. royal prerogative.',
    'If mandate: extraction can be justified as cosmic necessity; Pharaoh is source of Ma''at, not bound by it. If reciprocal: Pharaoh is subject to Ma''at constraints; extraction requires justification of benefit-sharing; failures indicate royal violation. This distinction determines whether the classification is Snare (mandate) or Tangled Rope (reciprocity).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_mandate_vs_reciprocal_obligation, empirical, 'Whether Ma''at is unidirectional mandate or reciprocal obligation').

omega_variable(
    suppression_mechanism_theological_vs_coercive,
    'Is the high suppression (0.82) enforced primarily through theological indoctrination and epistemic closure, or through coercive apparatus (secret police, execration texts, physical punishment)?',
    'Historical analysis of dissent documentation; examination of execration texts and damnatio memoriae practices; study of priestly education and scribal training curricula; comparison of theological suppression burden vs. military/police apparatus investment.',
    'If primarily theological: suppression can be overcome through cognitive reframing (alternative reading adoption). If primarily coercive: suppression persists even with reframing. This determines whether exit-option upgrading (from trapped to constrained) is feasible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_theological_vs_coercive, empirical, 'Whether suppression is theological indoctrination or coercive apparatus').

omega_variable(
    alternative_reading_survivorship,
    'Did alternative cosmological readings (reciprocity, distributed maintenance) persist as live intellectual traditions during the divine mandate reading''s institutional dominance, or were they completely suppressed?',
    'Textual analysis of wisdom literature, administrative correspondence, and private tomb inscriptions for heterodox claims; examination of Second Intermediate Period and Amarna Period sources for evidence of competing frameworks; analysis of scribal school curricula for range of cosmological positions taught.',
    'If alternatives persisted: the readings coexist; neither forecloses the other. If completely suppressed: the divine mandate reading forecloses alternatives through institutional power rather than logical contradiction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_reading_survivorship, empirical, 'Whether alternative Ma''at readings survived suppression or were eliminated').

omega_variable(
    reading_kerneled_vs_reading_constructed,
    'Is the ''divine mandate'' interpretation a reading of an actual ambiguous kernel (Ma''at as textual/symbolic concept that genuinely admits multiple interpretations), or is it a constructed framework retrofitted onto texts and practices?',
    'Chronological analysis: did the divine mandate reading emerge contemporaneously with Ma''at theological development (Kernel Reading), or did it crystallize later as institutional consolidation strengthened pharaonic power (constructed framework)? Examine earliest occurrences of divine mandate language vs. alternative cosmological framings in Old Kingdom through New Kingdom sources.',
    'If kerneled: this is a legitimate reading of an ambiguous cosmic principle. If constructed: the ''divine mandate'' reading is an institutional invention that naturalizes power concentration — it becomes a paradigm case of false summitry.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kerneled_vs_reading_constructed, conceptual, 'Whether divine mandate is a reading of a kernel or a constructed framework').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maat_order_principle__divine_mandate_reading, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maat_divine_theater_t0, maat_order_principle__divine_mandate_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(maat_divine_theater_t500, maat_order_principle__divine_mandate_reading, theater_ratio, 500, 0.62).
narrative_ontology:measurement(maat_divine_theater_t1000, maat_order_principle__divine_mandate_reading, theater_ratio, 1000, 0.65).

% Extraction over time
narrative_ontology:measurement(maat_divine_extract_t0, maat_order_principle__divine_mandate_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(maat_divine_extract_t500, maat_order_principle__divine_mandate_reading, base_extractiveness, 500, 0.61).
narrative_ontology:measurement(maat_divine_extract_t1000, maat_order_principle__divine_mandate_reading, base_extractiveness, 1000, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(maat_divine_supp_t0, maat_order_principle__divine_mandate_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(maat_divine_supp_t500, maat_order_principle__divine_mandate_reading, suppression_requirement, 500, 0.76).
narrative_ontology:measurement(maat_divine_supp_t1000, maat_order_principle__divine_mandate_reading, suppression_requirement, 1000, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maat_order_principle__divine_mandate_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(maat_order_principle__divine_mandate_reading, 0.18).
narrative_ontology:affects_constraint(maat_order_principle__divine_mandate_reading, maat_order_principle__reciprocity_reading).
narrative_ontology:affects_constraint(maat_order_principle__divine_mandate_reading, maat_order_principle__distributed_maintenance_reading).

% DUAL FORMULATION NOTE:
% The Ma'at order principle decomposes into three structurally distinct constraint stories representing three readings of an ambiguous kernel. Each reading instantiates different ε values, different beneficiary/victim structures, and different mechanisms of suppression. The divine mandate reading (this story) presents Ma'at as unidirectional cosmic necessity flowing through the Pharaoh; the reciprocity reading presents Ma'at as a binding constraint on the Pharaoh; the distributed maintenance reading presents Ma'at as collective responsibility. These are not three perspectives on one constraint — they are three different constraints grounded in different interpretations of the same kernel. Link them via affects_constraints to enable contamination propagation analysis and false summit detection.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(maat_order_principle__divine_mandate_reading, institutional, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
