% ============================================================================
% CONSTRAINT STORY: endogenous_reinterpretation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_endogenous_reinterpretation_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: endogenous_reinterpretation_reading
 *   human_readable: The 1890 Manifesto as Endogenous Prophetic Reinterpretation
 *   domain: religious_institutional_history/commitment_systems
 *
 * SUMMARY:
 *   The 1890 Manifesto issued by Wilford Woodruff as president of the Church
 *   of Jesus Christ of Latter-day Saints announced the church's official
 *   discontinuance of plural marriage, a practice foundational to the
 *   church's theology and institutional identity since the 1840s. Under the
 *   endogenous reinterpretation reading, this Manifesto represents a
 *   legitimate exercise of prophetic authority: God revealed the temporal
 *   suspension of plural marriage to preserve the church's salvific mission
 *   in the face of federal prosecution, territorial disincorporation, and
 *   institutional survival pressure. The constraint under this reading is
 *   pure coordination (Rope) — the church realigns around a new prophetic
 *   directive. However, the structural effects of this reinterpretation
 *   create asymmetric costs for those committed to the original reading:
 *   excommunication, loss of temple ordinances, social ostracism, and the
 *   experience of doctrine reversal. This reading instantiates one pole of a
 *   contested kernel: the plural marriage mandate itself can be read as
 *   eternally binding (exogenous override reading: the Manifesto is
 *   unauthorized overreach), as an institutional pragmatism (institutional
 *   pragmatism reading: prophetic authority was invoked to solve a real
 *   institutional problem), or as endogenous reinterpretation (this reading:
 *   legitimate prophetic reframing of a practice in light of changed
 *   conditions). The three readings coexist across different factions of the
 *   religious tradition, each grounding legitimacy in different interpretive
 *   premises about prophetic authority and doctrine revision.
 *
 * KEY AGENTS:
 *   - Institutional Church Leadership: Primary beneficiary (institutional/arbitrage) — retains organizational viability, temple operations, missionary capacity, and authority claims through the Manifesto reframing
 *   - Fundamentalist Practitioners: Primary victim (powerless/trapped, identity_locked at some power levels) — face excommunication, loss of temple access, doctrine reversal, and identity dissolution; identity-locked because commitment to the original reading is constitutive of their religious identity
 *   - Temple Operation System: Derivative beneficiary (institutional/arbitrage) — temple work continues and expands under the new reading without the federal legal threat that surrounded polygamy
 *   - Scholarly Theologians: Secondary observer (analytical/mobile) — see the Manifesto as legitimate theological work within a living prophetic tradition
 *   - Fundamentalist Organizations (FLDS, etc.): Organized victims (organized/constrained) — maintain the original reading through organizational separation; experience the Manifesto as constraint with potential sunset if conditions change
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(endogenous_reinterpretation_reading, 0.35).
domain_priors:suppression_score(endogenous_reinterpretation_reading, 0.42).
domain_priors:theater_ratio(endogenous_reinterpretation_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(endogenous_reinterpretation_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(endogenous_reinterpretation_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(endogenous_reinterpretation_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(endogenous_reinterpretation_reading, rope).
narrative_ontology:human_readable(endogenous_reinterpretation_reading, "The 1890 Manifesto as Endogenous Prophetic Reinterpretation").
narrative_ontology:topic_domain(endogenous_reinterpretation_reading, "religious_institutional_history/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(endogenous_reinterpretation_reading, '8c140f1d-3ed2-4979-b2cd-9af0b89f20ba').
narrative_ontology:cs_created_at('8c140f1d-3ed2-4979-b2cd-9af0b89f20ba', '').
narrative_ontology:cs_kernel_codification('8c140f1d-3ed2-4979-b2cd-9af0b89f20ba', fixed_text).
narrative_ontology:cs_authority_grounding('8c140f1d-3ed2-4979-b2cd-9af0b89f20ba', lineage).
narrative_ontology:cs_interpretation_layer_present('8c140f1d-3ed2-4979-b2cd-9af0b89f20ba').
narrative_ontology:cs_kernel_id(endogenous_reinterpretation_reading, plural_marriage_mandate).
narrative_ontology:cs_reading_relation('8c140f1d-3ed2-4979-b2cd-9af0b89f20ba', exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('8c140f1d-3ed2-4979-b2cd-9af0b89f20ba', institutional_pragmatism_reading, coexists_with).
narrative_ontology:cs_axiom('8c140f1d-3ed2-4979-b2cd-9af0b89f20ba', foundational, prophetic_succession_authority_remains_valid).
narrative_ontology:cs_axiom_status(prophetic_succession_authority_remains_valid, holdable).
narrative_ontology:cs_axiom_grounding('8c140f1d-3ed2-4979-b2cd-9af0b89f20ba', prophetic_succession_authority_remains_valid, conventional).
narrative_ontology:cs_axiom('8c140f1d-3ed2-4979-b2cd-9af0b89f20ba', foundational, doctrine_contingent_on_historical_conditions).
narrative_ontology:cs_axiom_status(doctrine_contingent_on_historical_conditions, holdable).
narrative_ontology:cs_axiom_grounding('8c140f1d-3ed2-4979-b2cd-9af0b89f20ba', doctrine_contingent_on_historical_conditions, instrumental).
narrative_ontology:cs_reference_frame('8c140f1d-3ed2-4979-b2cd-9af0b89f20ba', living_prophetic_authority_framework).
narrative_ontology:cs_drift_state('8c140f1d-3ed2-4979-b2cd-9af0b89f20ba', contemporary_post_manifesto_era, gap(practice_drift, substantial, true)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(endogenous_reinterpretation_reading, institutional_church).
narrative_ontology:constraint_beneficiary(endogenous_reinterpretation_reading, temple_operation_capacity).
narrative_ontology:constraint_beneficiary(endogenous_reinterpretation_reading, missionary_expansion).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INSTITUTIONAL CHURCH (ROPE) — The 1890 Manifesto, under this reading, represents legitimate exercise of prophetic authority to reinterpret doctrine in response to civilizational conditions. The church coordinates around a new understanding: plural marriage's suspension is a divine directive that preserves the church's salvific mission and organizational viability. The constraint is pure coordination — agents align their expectations around the prophetic reframing. No extraction from the institutional perspective; the beneficiary set (temple access, missionary work, institutional survival) flows through the coordination mechanism itself.
constraint_indexing:constraint_classification(endogenous_reinterpretation_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 2: FUNDAMENTALIST MEMBERSHIP (TANGLED ROPE) — Agents committed to the original plural marriage reading as a binding doctrinal obligation experience the Manifesto as a constraint that both coordinates institutional survival AND extracts their exclusion from temple ordinances and church authority. The constraint has genuine coordination content: it solves the institutional viability problem that federal pressure created. But from this perspective, that coordination benefit flows overwhelmingly to the institutional structure, while the cost (excommunication, doctrine reversal, identity dissolution) falls on faithful adherents of the prior reading. Identity-locked exit: the fundamentalist agent cannot exercise the constrained-level cost option (simply disagree and stay) because their identity is constituted through the commitment to the prior reading.
constraint_indexing:constraint_classification(endogenous_reinterpretation_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SCHOLARLY OBSERVER (ROPE) — From a distance view, the Manifesto represents a genuine coordination solution to an authentic dilemma: maintain doctrine without institutional survival, or preserve institutional viability through doctrinal reinterpretation. The scholarly observer sees the constraint as enabling ongoing theological work and religious community formation. The reinterpretation is not extraction but rather the exercise of a legitimate institutional authority (prophetic succession) to navigate contradictory demands. Low suppression from this perspective — the mechanism (prophetic reframing) is transparent and coherently explained.
constraint_indexing:constraint_classification(endogenous_reinterpretation_reading, rope,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: EXCOMMUNICATED FUNDAMENTALIST (SNARE) — An agent who has structured their entire family life, religious identity, and community participation around the original plural marriage mandate faces the Manifesto as pure extraction: loss of temple access, excommunication, social ostracism within the community, and no alternative institutional home that honors their original religious commitment. The coordination benefit (church survival) is entirely invisible from this perspective; only the cost (identity destruction, family fragmentation) is experienced. High suppression: alternative interpretations of prophetic authority are deemed illegitimate; remaining in the church requires abandoning the foundational commitment. Trapped exit: cannot remain in the community while maintaining the original reading, and no parallel institutional structure offers the same salvific claims.
constraint_indexing:constraint_classification(endogenous_reinterpretation_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 5: FUNDAMENTALIST ORGANIZATION (SCAFFOLD) — Organized groups (FLDS, related splinter movements) see the Manifesto as temporary suspension of a binding practice pending prophetic reversal or institutional collapse. They coordinate around sustaining the original reading by maintaining separate communities. From this perspective, the constraint is a temporary coordination failure being managed through organizational exit, not suppression. Theater is moderate: the fundamentalist groups' own authority structures (claims of prophetic continuation or preservation) create parallel institutional theater. The scaffold structure emerges because the fundamentalist movements position themselves as sunset mechanisms: when institutional conditions change or the mainstream church fails, the original practice will be restored.
constraint_indexing:constraint_classification(endogenous_reinterpretation_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational view, the tension between foundational doctrine and institutional survival creates an immutable dilemma inherent to religious communities maintaining exclusive salvific claims while embedded in secular legal orders. The Manifesto emerges as a natural structural response to this immutable pressure. Some reinterpretation is inevitable; the specific form (prophetic reframing) is one natural resolution. However, this mountain classification risks naturalizing what is actually a contingent choice among multiple possible institutional responses — the constraint is a real structural pressure, not a law of nature.
constraint_indexing:constraint_classification(endogenous_reinterpretation_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(endogenous_reinterpretation_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(endogenous_reinterpretation_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(endogenous_reinterpretation_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(endogenous_reinterpretation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate, reflecting genuine coordination with asymmetric costs. The constraint solves a real institutional dilemma (survive under federal pressure or maintain doctrine without institutional form). Under the endogenous reading, the solution is legitimate — a new prophetic directive realigns the entire community. However, the cost to those committed to the original reading is substantial: loss of ordinances, excommunication, social ostracism. The extractiveness measures the degree to which the reinterpretation benefits the institutional structure at the cost of prior commitments. Rising from 0.15 to 0.35 over the interval reflects accumulating costs as the implementation hardens (excommunication of resisting members, loss of temple access for non-compliant families). Suppression (0.42): Moderate. The mechanism is not hidden — the Manifesto is publicly announced, theologically explained, and justified through prophetic authority. However, suppression operates through the mechanism of declaring alternative readings illegitimate: to maintain institutional membership, agents must accept the reinterpretation. Remaining in the church requires abandoning the original reading, creating suppression through conformity pressure rather than coercion. Theater ratio (0.38): Moderate. The Manifesto includes genuine theological work (reinterpreting doctrine, reconciling apparent contradictions) and genuine institutional problem-solving (negotiating federal pressure), but also performative elements (the claim of revelation rather than institutional decision-making, the theological language masking political necessity).
 *
 * PERSPECTIVAL GAP:
 *   The institutional perspective (rope) sees the Manifesto as solving a genuine coordination problem: the church realigns around a new prophetic directive that preserves institutional viability while remaining theologically coherent. The beneficiary set (institutional survival, temple operations, missionary expansion) flows through the coordination mechanism. The fundamentalist perspective (snare/tangled rope) sees the same Manifesto as extraction: loss of temple ordinances, loss of community, loss of the identity they constructed through the prior commitment. The coordination benefit (institutional survival) is invisible from this perspective; only the cost is experienced. The scholarly observer (rope) sees a legitimate theological exercise: the church's prophetic authority enables reinterpretation in response to civilizational pressure. The analytical perspective risks a mountain classification by naturalizing the institutional accommodation as an inevitable law of prophetic religion, obscuring the contingent choice among multiple possible responses. The perspectival gap reveals that the constraint's classification depends on whether the observer experiences the benefits of coordination (rope) or only the costs of prior commitment reversal (snare/tangled rope).
 *
 * DIRECTIONALITY LOGIC:
 *   The institutional perspective (institutional/arbitrage) derives d ≈ 0.10-0.15 from beneficiary status + arbitrage exit options: the church leadership can maintain institutional authority and operational continuity through the Manifesto reframing. The fundamentalist perspective (powerless/trapped) derives d ≈ 0.90-0.95 from victim status + trapped exit: the excommunicated fundamentalist faces maximum extraction with no exit pathway that preserves both the original commitment and community membership. The organized fundamentalist perspective (organized/constrained) derives d ≈ 0.60-0.70 from victim status + constrained exit: splinter groups can maintain the original reading but at the cost of institutional separation. The scholarly observer (analytical/mobile) derives d ≈ 0.50 from observer position + mobile exit: the analyst can engage or disengage from either reading without structural consequences. These directionality values feed the sigmoid f(d) to produce the effective extractiveness chi experienced by each agent. The rope classification holds across institutional and scholarly perspectives because their f(d) values yield low or balanced chi; the tangled rope and snare classifications emerge for victims because their f(d) values yield high chi despite the coordination content of the constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prophetic_authority_grounding,
    'What grounds the legitimacy of the 1890 Manifesto as a genuine prophetic utterance versus a pragmatic institutional choice retro-fitted with prophetic language?',
    'Historical analysis of decision-making processes, archival evidence of deliberation vs. claimed revelation sequence, comparison with prior revelation claims in the tradition, theological examination of whether the reinterpretation logically flows from prior doctrinal commitments',
    'If grounded in genuine revelation: the reading''s foundational axiom (prophetic_succession_authority_remains_valid) is holdable and grounds the rope classification. If pragmatic institutional choice: the constraint becomes tangled_rope or snare depending on victims'' perspectives, and the axiom becomes overridden by recognition of institutional pressures.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(prophetic_authority_grounding, conceptual, 'Whether the Manifesto represents genuine revelation or pragmatic institutional reframing').

omega_variable(
    doctrine_suspension_versus_abrogation,
    'Is plural marriage eternally suspended (temporarily withdrawn but doctrinal foundation remains) or abrogated (permanently revoked and doctrine revised)?',
    'Textual analysis of Manifesto language and subsequent official statements; examination of whether the constraint frames suspension as temporary or permanent; analysis of whether official doctrine shifted to make plural marriage non-essential versus merely postponed',
    'If suspension: the reading sustains the rope classification and validates the fundamentalist perspective that conditions could change and the practice return. If abrogation: the constraint becomes asymmetric extraction for fundamentalists (their commitment was revoked without their consent), and the rope classification for the institutional perspective requires reclassification to account for permanent doctrine revision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_suspension_versus_abrogation, empirical, 'Whether the Manifesto suspends or permanently abrogates the plural marriage doctrine').

omega_variable(
    kernel_reading_contest_structure,
    'Do the three sibling readings (endogenous_reinterpretation, exogenous_override, institutional_pragmatism) genuinely represent logically distinct positions, or do they collapse under scrutiny into different framings of the same institutional accommodation?',
    'Comparative analysis of the three readings'' foundational axioms and reference frames. If the axioms involve contradictory empirical or normative claims (e.g., one claims revelation, another denies it), they coexist. If they represent different emphasis within a single institutional narrative, they may influence rather than coexist.',
    'If logically distinct: the reading_relations are correctly set to coexists_with. If they collapse: the kernel decomposition itself requires revision, and this reading may need reclassification relative to siblings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_structure, conceptual, 'Whether sibling readings are logically independent or alternative framings of one accommodation').

omega_variable(
    identity_locked_exit_mechanism,
    'For the fundamentalist agent experiencing identity-locked exit, what specific component of identity fusion prevents recognition that constrained-cost exit is available? Is it fusion with the original doctrine itself, fusion with the church''s authority claims, or fusion with a relational identity (spouse, parent, community member)?',
    'Ethnographic and testimonial analysis of excommunicated and splinter-group members; examination of post-exit narratives to identify which identity components persisted and which dissolved',
    'If doctrine-fusion: the fundamentalist agent could theoretically maintain identity outside the church. If authority-fusion: they cannot maintain their own religious authority claims without the institutional structure. If relational-fusion: the agent''s family and community identity is shattered by exit. Different identity lock types have different implications for whether exit is structurally mobile (identity_locked) or genuinely trapped.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_exit_mechanism, empirical, 'The specific structure of identity fusion preventing fundamentalist exit').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(endogenous_reinterpretation_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(endogenous_theater_t0, endogenous_reinterpretation_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(endogenous_theater_t5, endogenous_reinterpretation_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(endogenous_theater_t10, endogenous_reinterpretation_reading, theater_ratio, 10, 0.38).

% Extraction over time
narrative_ontology:measurement(endogenous_extractiveness_t0, endogenous_reinterpretation_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(endogenous_extractiveness_t5, endogenous_reinterpretation_reading, base_extractiveness, 5, 0.25).
narrative_ontology:measurement(endogenous_extractiveness_t10, endogenous_reinterpretation_reading, base_extractiveness, 10, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(endogenous_reinterpretation_reading, identity_coordination).
narrative_ontology:affects_constraint(endogenous_reinterpretation_reading, exogenous_override_reading).
narrative_ontology:affects_constraint(endogenous_reinterpretation_reading, institutional_pragmatism_reading).
narrative_ontology:affects_constraint(endogenous_reinterpretation_reading, federal_polygamy_prosecution_pressure).
narrative_ontology:affects_constraint(endogenous_reinterpretation_reading, temple_ordinance_legitimacy).

% DUAL FORMULATION NOTE:
% The plural_marriage_mandate kernel decomposes into three constraint stories, each instantiating a different reading with different ε values and victim/beneficiary structures. The endogenous_reinterpretation_reading frames the Manifesto as legitimate prophetic exercise (ε=0.35, rope). The exogenous_override_reading frames it as unauthorized override (ε=0.70, snare from the fundamentalist perspective). The institutional_pragmatism_reading treats it as institutional accommodation with contingent prophetic justification (ε=0.55, tangled rope). Each story has its own perspectives and network relationships. Decomposition follows the ε-invariance principle: the three readings have materially different ε values reflecting different empirical claims (whether revelation occurred, whether the practice is permanently abrogated, whether institutional pressure was the true driver).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
