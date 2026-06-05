% ============================================================================
% CONSTRAINT STORY: second_amendment_scope__collective_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_scope__collective_right_reading, []).

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
 *   constraint_id: second_amendment_scope__collective_right_reading
 *   human_readable: Second Amendment as Collective State Militia Authority (Institutional Reading)
 *   domain: constitutional_law/political_theory/rights_jurisprudence
 *
 * SUMMARY:
 *   This constraint represents ONE reading of the contested Second Amendment
 *   kernel — specifically, the collective-right reading that interprets the
 *   Second Amendment as protecting state militia authority rather than
 *   individual firearms ownership rights. Under this reading, the prefatory
 *   clause ('A well regulated Militia, being necessary to the security of a
 *   free State') is binding and determinative: it limits the operative clause
 *   ('the right of the people to keep and bear Arms, shall not be infringed')
 *   to militia-connected purposes only. The beneficiary set is restricted to
 *   state governments, militia authorities, and the federal government's
 *   coordination with state forces. Individuals who claim firearms ownership
 *   unconnected to militia service are excluded from constitutional
 *   protection — they are classified as victims of the constraint, not as
 *   holders of rights. This reading exhibits tangled-rope characteristics: it
 *   coordinates state militia authority (genuine collective action problem
 *   solved) while simultaneously extracting individual ownership claims from
 *   the domain of constitutional protection. The extractiveness is moderate
 *   (0.28) because the constraint operates narrowly — it doesn't extract
 *   resources, only scope. But suppression is real (0.42): alternative
 *   constitutional readings must overcome textual and doctrinal barriers
 *   rooted in this reading's interpretation. Theater has increased
 *   historically (0.38 → 0.55 over the 100-year interval) as the founding-era
 *   militia structures that the reading references have increasingly become
 *   formalized and rhetorical rather than functionally equivalent to
 *   contemporary organized armed forces.
 *
 * KEY AGENTS:
 *   - State Militia Authorities / National Guard Systems: Primary beneficiary (institutional/arbitrage) — gain constitutional legitimacy for militia regulation and control; lowest extraction experienced
 *   - Individual Firearms Claimants: Primary victim (powerless/trapped) — excluded from the beneficiary set; structurally unable to claim constitutional right outside militia framework
 *   - Federal Government: Secondary beneficiary/victim (institutional/constrained) — gains coordination with state militia authority but loses regulatory flexibility for individual-level firearms policy
 *   - Militia Service Participants: Secondary victim (moderate/constrained) — included in beneficiary set but only as participants in state-directed militia structures; individual autonomy constrained
 *   - Gun Regulation Advocacy Coalition: Tertiary beneficiary (organized/mobile) — uses this reading to legitimize broad regulatory authority over firearms
 *   - Originalist/Textualist Jurisprudential Community: Institutional actor (institutional/arbitrage) — maintains interpretive authority through this reading; benefits from foreclosure of competing interpretations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_scope__collective_right_reading, 0.28).
domain_priors:suppression_score(second_amendment_scope__collective_right_reading, 0.42).
domain_priors:theater_ratio(second_amendment_scope__collective_right_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_scope__collective_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_scope__collective_right_reading, "Second Amendment as Collective State Militia Authority (Institutional Reading)").
narrative_ontology:topic_domain(second_amendment_scope__collective_right_reading, "constitutional_law/political_theory/rights_jurisprudence").

domain_priors:requires_active_enforcement(second_amendment_scope__collective_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_scope__collective_right_reading, '7b6c6ab8-7c12-4424-9dbe-9c9bdbcf3b90').
narrative_ontology:cs_kernel_codification('7b6c6ab8-7c12-4424-9dbe-9c9bdbcf3b90', fixed_text).
narrative_ontology:cs_authority_grounding('7b6c6ab8-7c12-4424-9dbe-9c9bdbcf3b90', lineage).
narrative_ontology:cs_interpretation_layer_present('7b6c6ab8-7c12-4424-9dbe-9c9bdbcf3b90').
narrative_ontology:cs_reading_relation('7b6c6ab8-7c12-4424-9dbe-9c9bdbcf3b90', second_amendment_scope__individual_right_reading, forecloses).
narrative_ontology:cs_reading_relation('7b6c6ab8-7c12-4424-9dbe-9c9bdbcf3b90', second_amendment_scope__civic_right_reading, influences).
narrative_ontology:cs_axiom('7b6c6ab8-7c12-4424-9dbe-9c9bdbcf3b90', foundational, collective_right_only).
narrative_ontology:cs_axiom_status(collective_right_only, overridden).
narrative_ontology:cs_axiom_grounding('7b6c6ab8-7c12-4424-9dbe-9c9bdbcf3b90', collective_right_only, empirically_contingent).
narrative_ontology:cs_axiom('7b6c6ab8-7c12-4424-9dbe-9c9bdbcf3b90', foundational, prefatory_clause_is_limiting).
narrative_ontology:cs_axiom_status(prefatory_clause_is_limiting, holdable).
narrative_ontology:cs_axiom_grounding('7b6c6ab8-7c12-4424-9dbe-9c9bdbcf3b90', prefatory_clause_is_limiting, empirically_contingent).
narrative_ontology:cs_reference_frame('7b6c6ab8-7c12-4424-9dbe-9c9bdbcf3b90', founding_era_state_militia_supremacy).
narrative_ontology:cs_drift_state('7b6c6ab8-7c12-4424-9dbe-9c9bdbcf3b90', post_heller_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('7b6c6ab8-7c12-4424-9dbe-9c9bdbcf3b90', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(second_amendment_scope__collective_right_reading, second_amendment_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_scope__collective_right_reading, state_militia_authority).
narrative_ontology:constraint_beneficiary(second_amendment_scope__collective_right_reading, state_regulatory_apparatus).
narrative_ontology:constraint_victim(second_amendment_scope__collective_right_reading, individual_firearms_ownership_claims).
narrative_ontology:constraint_victim(second_amendment_scope__collective_right_reading, extraconstitutional_ownership_rights).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL RIGHTS CLAIMANT (SNARE) — Under this reading, individuals have no constitutional claim to firearms ownership outside the militia framework. They are trapped: the constraint excludes them from the beneficiary set entirely and admits no individual exit route. They bear the full cost of narrow constitutional scope with no corresponding benefit or escape path.
constraint_indexing:constraint_classification(second_amendment_scope__collective_right_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: STATE MILITIA AUTHORITY (ROPE) — State governments and their militia structures (National Guard, state forces) are the beneficiaries under this reading. They enjoy constitutional authority to regulate, organize, and control militia forces. The constraint functions as a coordination mechanism for state authority: it legitimizes state control over organized armed forces and enables standardized regulation. Net benefit with institutional exit options — states can exercise this authority or defer to federal frameworks.
constraint_indexing:constraint_classification(second_amendment_scope__collective_right_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: MILITIA PARTICIPANTS (TANGLED ROPE) — Citizens who participate in organized militia structures (National Guard, etc.) experience both coordination and extraction. The constraint coordinates their participation in collective security while simultaneously extracting obedience and potentially restricting civilian autonomy. They benefit from the institutional framework but at the cost of state authority over their armed status. Exit is constrained but possible (leave militia service at personal cost).
constraint_indexing:constraint_classification(second_amendment_scope__collective_right_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: FEDERAL GOVERNMENT (TANGLED ROPE) — Federal authority experiences mixed coordination and extraction under this reading. The collective reading coordinates militia authority with state power structures, enabling federal-state cooperation on security. But it also constrains federal authority over individual firearms regulation if the right is collective rather than individual — federal power is checked by the state militia focus. Extraction runs in both directions; exit is constrained by constitutional framwork.
constraint_indexing:constraint_classification(second_amendment_scope__collective_right_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: FOUNDING-ERA MILITIA STRUCTURE (PITON) — The institutional arrangement referenced (state-based militia systems from the 18th century) is substantially degraded and performative. Modern National Guard structures, Reserve forces, and state militia organizations function differently from the founding militia conception. The constraint's reference to militia authority persists through institutional inertia and formalized doctrine, but the actual militia structures have transformed. Theater_ratio is high because the constitutional language references an organizational reality that no longer exists in its original form.
constraint_indexing:constraint_classification(second_amendment_scope__collective_right_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / TEXTUAL NATURAL LAW VIEW (MOUNTAIN) — From a purely textual-originalist position, the explicit language 'A well regulated Militia, being necessary to the security of a free State' grounds the right in state militia authority, making individual ownership claims structurally impossible. This perspective treats the text as establishing an immutable logical constraint: the prefatory clause determines the operative clause's scope. However, this classification risks being a false summit: the claim that the text's plain meaning is self-evident and unchangeable naturalizes what is actually an interpretive choice about how to read the English language and historical intent.
constraint_indexing:constraint_classification(second_amendment_scope__collective_right_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: GUN REGULATION COALITION (ROPE) — Organized actors who support broad regulatory authority (public health advocates, gun control organizations) see this reading as a coordination mechanism that legitimizes state authority to regulate firearms comprehensively. They experience the constraint as enabling their policy goals through legitimate constitutional framework. Exit is mobile (they could adopt other constitutional theories) but they choose to remain invested in this reading because it coordinates their interests with state authority.
constraint_indexing:constraint_classification(second_amendment_scope__collective_right_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_scope__collective_right_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(second_amendment_scope__collective_right_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(second_amendment_scope__collective_right_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(second_amendment_scope__collective_right_reading, TR),
    TR >= 0.70.

:- end_tests(second_amendment_scope__collective_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-moderate. The collective reading does not extract resources or labor — it extracts legal scope. Individuals claiming ownership rights are excluded from the constitutional beneficiary set entirely, so the extraction is purely classificatory and normative, not economic. However, the extraction is real: it denies a claimed right. The moderate value (not minimal) reflects that this is a genuine deprivation of legal status, not merely a negligible limitation. Suppression (0.42): Moderate. Substantial doctrinal and institutional barriers prevent the individual-right reading from gaining traction once this collective reading is established. The prefatory clause interpretation creates a textual gate that must be overcome. However, suppression is not total (not 0.60+): the individual reading has never been fully eliminated from legal discourse, and competing interpretations are actively held by significant jurisprudential communities. Theater ratio (0.55): Moderate-high and rising. The founding-era militia structures referenced in this reading no longer exist in their original form. Modern National Guard, Reserve, and state militia organizations are formalized, professionalized, and structurally different from decentralized colonial militia. The constitutional language refers to an institutional reality that has been substantially transformed, making the constraint increasingly performative — it invokes militia authority while the actual militia structures bear limited resemblance to the invoked concept. The historical trend (0.38 → 0.55) reflects this gap widening as contemporary institutions diverge further from founding concepts.
 *
 * PERSPECTIVAL GAP:
 *   This reading produces radically different classifications from its siblings. Where the individual-right reading sees individual gun owners as beneficiaries (and classifies as Rope or Scaffold from their perspective), this collective reading classifies them as victims (Snare). Where civic-right reading sees militia participation as conditioning individual rights, this reading sees militia authority as the sole legitimate beneficiary, with individuals entirely excluded. The piton perspective reveals the constraint's reference object (founding-era militia) has become substantially inoperative, suggesting the reading may be maintaining performative authority over a degraded institutional reality. The mountain perspective risks naturalizing a textual interpretive choice (prefatory clauses are determinative) as an immutable law of constitutional grammar. The analytical observer's key task is recognizing that this perspectival distribution is diagnostic: if all seven perspectives produce the same classification, the constraint would be uniform-type and require no perspectival analysis. The fact that they diverge across all six types signals that the indexical position (power, time, exit, scope) fundamentally changes how the constraint appears.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality in this constraint follows the beneficiary/victim structure. State militia authorities and the federal government derive low d (0.15-0.25) as beneficiaries with institutional exit options (arbitrage) — the constraint enables their authority and creates coordination functions they value. Individuals excluded from the beneficiary set experience high d (0.85-0.95) as trapped agents — they cannot exercise the right because the constraint defines it as unavailable to them outside militia service. Militia participants occupy middle ground: d ≈ 0.50-0.55 (symmetric) because they benefit from the institutional coordination but suffer extraction of autonomy. The gun regulation coalition experiences low d (0.20-0.30) as organized beneficiaries whose policy interests align with broad state regulatory authority. The analytical observer derives standard d ≈ 0.72 (analytical position). The perspectivist structure means chi = ε × f(d) × σ(S) produces dramatically different experienced extractiveness depending on the agent: beneficiary institutional actors experience negative or near-zero chi (the constraint subsidizes their authority); trapped individuals experience high chi (1.15+); moderate agents experience intermediate chi (0.65-0.75). This directionality structure is the source of the perspectival gap.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves mandatrophy by explicitly instantiating one kernel reading rather than claiming to be the universal correct type. The tangled_rope classification (mixing coordination of militia authority with extraction of individual scope) is the binding classification from the analytical position — it shows genuine coordination function (state militia) existing alongside asymmetric extraction (individual rights foreclosure). If the reading were pure rope, it would be claiming that individual exclusion is merely a coordination side effect, not an extraction. If it were pure snare, it would be claiming the entire constraint is extractive with no genuine coordination. Tangled_rope correctly captures that state militia authority coordination is real and genuine, while individual deprivation is real and extractive. The perspectival distribution across all six types (mountain, rope, tangled_rope, snare, scaffold, piton) demonstrates that the constraint's type depends entirely on the observer's position — there is no single 'true' type, only a presheaf of readings. This is the diagnostic signature of a kernel reading: mandatrophy dissolves into perspectival relativism when one recognizes that different readings of the same constitutional kernel produce different structural properties.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_ambiguity_prefatory_clause,
    'Does the prefatory clause (''A well regulated Militia, being necessary to the security of a free State'') limit the operative clause (''the right of the people to keep and bear Arms, shall not be infringed'') or merely provide context?',
    'Comparative analysis of 18th-century legal texts with similar structure; linguistic analysis of dependent vs. independent clauses in constitutional drafting; historical evidence of framers'' intent regarding scope limitations',
    'If prefatory clause is limiting: individual ownership claims are foreclosed, this reading stands. If prefatory clause is merely contextual: operative clause permits individual rights, siblings'' readings become viable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(textual_ambiguity_prefatory_clause, empirical, 'Interpretive status of the prefatory clause as scope-limiter vs. context-provider').

omega_variable(
    militia_definition_temporal_stability,
    'What counts as a ''well regulated Militia'' — is it the founding-era militia concept (decentralized state-based forces), modern National Guard structures, or any organized military body?',
    'Historical analysis of militia organization across founding era, 19th century, and modern period; state constitutional provisions defining militia; comparison with international armed force terminology and structures',
    'If founding-era militia only: constraint becomes piton (contemporary structures don''t match). If modern structures count: constraint remains functional but requires updating beneficiary set. If definition includes private militia: extraction boundaries shift dramatically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_definition_temporal_stability, empirical, 'Definition and temporal stability of ''well regulated Militia'' concept').

omega_variable(
    framers_intent_individual_vs_collective_right,
    'Did the framers intend the Second Amendment to protect an individual''s right to bear arms outside militia service, or exclusively state militia authority?',
    'Textual analysis of founding documents, Federalist Papers, state ratification debates, founding-era militia laws, and contemporary legal commentary; comparison with framing of other rights (First, Third amendments) regarding individual vs. collective beneficiaries',
    'If individual intent confirmed: this reading''s foundational axiom (collective_right_only) is overridden, reading collapses into sibling civic reading or foreclosed entirely. If collective intent confirmed: this reading''s axiom is holdable, sibling readings face pressure to justify competing framers'' intent claims.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(framers_intent_individual_vs_collective_right, empirical, 'Historical evidence of framers'' intent: individual right vs. collective state authority').

omega_variable(
    false_summit_natural_law_risk,
    'Is the claim that prefatory clauses determine scope a natural law of constitutional grammar, or a chosen interpretive method that could be discarded?',
    'Examination of other constitutional provisions with prefatory structures; comparison of prefatory-clause interpretations across federal and state constitutions; analysis of whether alternative hermeneutics produce logically consistent results',
    'If natural law: mountain classification is correct, individual reading is structurally impossible. If chosen method: mountain is false summit, individual reading remains live option, constraint reclassifies to tangled_rope or rope based on empirical interpretive distribution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law_risk, conceptual, 'Whether prefatory-clause interpretation is natural law or contestable hermeneutic method').

omega_variable(
    heller_precedent_foreclosure_status,
    'Does the Supreme Court''s 2008 Heller decision (recognizing individual right) permanently foreclose this collective-right reading within American constitutional law, or does it remain a live jurisprudential option despite precedent?',
    'Analysis of stare decisis doctrine, precedent stability, overruling possibilities, and whether Heller''s reasoning logically eliminates collective reading or merely rejects it as policy choice; examination of minority and dissenting opinions asserting collective reading',
    'If Heller forecloses: this reading''s axiom shifts from holdable to overridden (within American law tradition). If Heller rejects but does not foreclose: reading remains holdable, precedent creates institutional pressure but not logical elimination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(heller_precedent_foreclosure_status, empirical, 'Heller precedent''s foreclosure status relative to collective-right reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_scope__collective_right_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(collective_reading_theater_t0, second_amendment_scope__collective_right_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(collective_reading_theater_t50, second_amendment_scope__collective_right_reading, theater_ratio, 50, 0.48).
narrative_ontology:measurement(collective_reading_theater_t100, second_amendment_scope__collective_right_reading, theater_ratio, 100, 0.55).

% Extraction over time
narrative_ontology:measurement(collective_reading_extract_t0, second_amendment_scope__collective_right_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(collective_reading_extract_t50, second_amendment_scope__collective_right_reading, base_extractiveness, 50, 0.24).
narrative_ontology:measurement(collective_reading_extract_t100, second_amendment_scope__collective_right_reading, base_extractiveness, 100, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_scope__collective_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_scope__collective_right_reading, second_amendment_scope__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_scope__collective_right_reading, second_amendment_scope__civic_right_reading).

% DUAL FORMULATION NOTE:
% All three readings of kernel second_amendment_scope share the same text (Second Amendment) but interpret its logical structure differently. The collective reading prioritizes the prefatory clause as determinative; the civic reading makes the operative clause primary but conditions it on militia connection; the individual reading treats the operative clause as independent. Each produces a distinct constraint with different beneficiary/victim structures and different ε values. They are linked via network.affects_constraints in a triangular structure: each reading influences the others by establishing or challenging the textual interpretive gate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
