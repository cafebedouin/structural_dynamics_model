% ============================================================================
% CONSTRAINT STORY: collective_militia_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_collective_militia_reading, []).

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
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: collective_militia_reading
 *   human_readable: Second Amendment as Collective Militia Right (State Regulatory Authority Reading)
 *   domain: constitutional_law/federalism/political_theory
 *
 * SUMMARY:
 *   The collective militia reading interprets the Second Amendment as
 *   granting regulatory authority exclusively to state governments over
 *   militia formation and maintenance, with the prefatory clause ('well
 *   regulated Militia, being necessary to the security of a free State')
 *   determining the operative clause's scope entirely. Under this reading,
 *   individual possession of firearms falls outside the Amendment's
 *   protective scope — individuals possess no constitutional right to bear
 *   arms for self-defense, hunting, sport, or other personal purposes. The
 *   beneficiaries are state governments and the federal system's federalism
 *   structure (which permits state-level regulation without constitutional
 *   constraint from an individual rights doctrine). The victims are
 *   individual gun owners, the firearms industry, and anyone claiming a
 *   personal constitutional right to armed self-defense. This reading creates
 *   extreme asymmetry: state governments can regulate, license, register,
 *   restrict, or prohibit civilian firearm ownership without violating the
 *   Second Amendment, because the Amendment speaks only to state militia
 *   purposes. The constraint exhibits high extractiveness (0.78) and high
 *   suppression (0.82) because the operative mechanism is constitutional
 *   denial: the reading forecloses the victim's entire claim to
 *   constitutional protection by interpreting the text as not addressing
 *   their interest at all. Theater ratio is relatively low (0.35) because the
 *   reading makes a straightforward textual argument (prefatory clause
 *   governs operative clause) rather than relying on procedural or
 *   institutional theater. However, post-Heller (2008), the reading's
 *   institutional position has degraded: it is maintained primarily through
 *   academic loyalty, political commitment, and state regulatory practice,
 *   not through Supreme Court doctrine. This qualifies as Piton dynamics from
 *   certain perspectives. The constraint is a kernel reading, one of three
 *   structurally distinct interpretations of the same constitutional text.
 *
 * KEY AGENTS:
 *   - State Governments (Legislative & Executive Branches): Primary beneficiaries (institutional/arbitrage) — retain plenary regulatory authority over civilian firearms with no Second Amendment constitutional constraint
 *   - Federal Government (Executive & Legislative Branches): Secondary beneficiary (institutional/constrained) — gains federalism clarity but constrained in direct federal gun control authority
 *   - Individual Gun Owners: Primary victims (powerless/trapped) — denied constitutional protection by the reading's textual interpretation; no exit option from constitutional claim
 *   - Firearms Industry: Secondary victim (organized/constrained) — faces sweeping state regulatory regimes; constrained exit through litigation, relocation, or political advocacy
 *   - Progressive Legal Establishment & Gun Control Organizations: Institutional maintainers (organized/constrained) — institutionally promote reading; face degraded doctrinal force post-Heller; maintain through inertia (Piton perspective)
 *   - Supreme Court (Post-Heller): Analytical authority (institutional/analytical) — has rejected collective reading; enforces individual rights doctrine despite historical plausibility of collective reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(collective_militia_reading, 0.78).
domain_priors:suppression_score(collective_militia_reading, 0.82).
domain_priors:theater_ratio(collective_militia_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(collective_militia_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(collective_militia_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(collective_militia_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(collective_militia_reading, snare).
narrative_ontology:human_readable(collective_militia_reading, "Second Amendment as Collective Militia Right (State Regulatory Authority Reading)").
narrative_ontology:topic_domain(collective_militia_reading, "constitutional_law/federalism/political_theory").

domain_priors:requires_active_enforcement(collective_militia_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(collective_militia_reading, 'cb74e855-4c0b-458f-ba2e-4ffdafe7fc06').
narrative_ontology:cs_created_at('cb74e855-4c0b-458f-ba2e-4ffdafe7fc06', '').
narrative_ontology:cs_kernel_codification('cb74e855-4c0b-458f-ba2e-4ffdafe7fc06', fixed_text).
narrative_ontology:cs_authority_grounding('cb74e855-4c0b-458f-ba2e-4ffdafe7fc06', lineage).
narrative_ontology:cs_interpretation_layer_present('cb74e855-4c0b-458f-ba2e-4ffdafe7fc06').
narrative_ontology:cs_kernel_id(collective_militia_reading, second_amendment_text).
narrative_ontology:cs_reading_relation('cb74e855-4c0b-458f-ba2e-4ffdafe7fc06', individual_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('cb74e855-4c0b-458f-ba2e-4ffdafe7fc06', sophisticated_collective_reading, influences).
narrative_ontology:cs_axiom('cb74e855-4c0b-458f-ba2e-4ffdafe7fc06', foundational, prefatory_clause_scope_determinative).
narrative_ontology:cs_axiom_status(prefatory_clause_scope_determinative, holdable).
narrative_ontology:cs_axiom('cb74e855-4c0b-458f-ba2e-4ffdafe7fc06', foundational, militia_purpose_exclusive).
narrative_ontology:cs_axiom_status(militia_purpose_exclusive, holdable).
narrative_ontology:cs_reference_frame('cb74e855-4c0b-458f-ba2e-4ffdafe7fc06', state_militia_regulation_framework).
narrative_ontology:cs_drift_state('cb74e855-4c0b-458f-ba2e-4ffdafe7fc06', post_heller_2008, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(collective_militia_reading, state_governments).
narrative_ontology:constraint_beneficiary(collective_militia_reading, federal_regulatory_authority).
narrative_ontology:constraint_victim(collective_militia_reading, individual_gun_owners).
narrative_ontology:constraint_victim(collective_militia_reading, firearms_industry).
narrative_ontology:constraint_victim(collective_militia_reading, personal_self_defense_claimants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL GUN OWNER (SNARE) — Under this reading, the Second Amendment provides no personal constitutional protection for firearm possession. The owner is trapped: the constitutional text itself (under the collective reading) denies the claim to individual self-defense as a constitutional right. Exit from this constraint would require abandoning the claim that the Constitution protects individual gun ownership — logically impossible for someone seeking constitutional recourse. Maximum extraction with maximum suppression: the victim cannot exercise the right they claim exists, and the constitutional text is read as foreclosing their claim entirely.
constraint_indexing:constraint_classification(collective_militia_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: STATE GOVERNMENTS / LEGISLATIVE BRANCH (ROPE) — Under this reading, state legislatures benefit directly: they retain plenary authority to regulate individual firearm possession without federal constitutional constraint. The Second Amendment poses no federalism barrier to state gun control, registration, prohibition, or licensing regimes. States experience this constraint as coordination: the prefatory clause clarifies that militia purposes dominate, legitimating state regulatory power. This is a beneficiary perspective with near-total exit optionality (arbitrage) — states can regulate firearms at will within the federalism framework. Low experienced extraction because extraction flows toward the state, not away.
constraint_indexing:constraint_classification(collective_militia_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: FIREARMS INDUSTRY & PRO-GUN ORGANIZATIONS (TANGLED ROPE) — These organized actors face constrained exit but also benefit from the constraint in complex ways. They benefit from regulatory clarity (state control is predictable, federally fragmented but legally stable once established in each state). They bear significant extraction through sweeping state regulatory authority. Their exit option is constrained: they can litigate the reading, organize politically, or relocate to more favorable state jurisdictions, but these carry high costs. Moderate-high extraction because both coordination (clear federal limits on the constraint) and asymmetric imposition coexist.
constraint_indexing:constraint_classification(collective_militia_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: FEDERAL GOVERNMENT / EXECUTIVE BRANCH (TANGLED ROPE) — The federal government under this reading can enforce cooperative federalism through conditional grants and spending power, but faces constitutional constraint against directly disarming state militias. The federal executive experiences both benefit (clear federalism boundary prevents state militias from becoming federal security threats, aligning with militia regulation authority) and constraint (cannot use Second Amendment as basis for nationwide gun control, must route authority through Commerce Clause, Necessary and Proper Clause, or spending conditions). Exit option is constrained because the constitutional reading itself limits federal strategies. Moderate extraction because both coordination (stable federalism) and regulatory constraint coexist.
constraint_indexing:constraint_classification(collective_militia_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / TEXTUALIST VIEW (MOUNTAIN) — From a civilizational scope, the collective reading can present itself as a natural law of constitutional interpretation: the prefatory clause logically determines the operative clause; militia purposes are immutable; individual rights interpretation is a modern deviation from the text's essential meaning. This perspective treats the reading as an inherent property of language and logic. However, the structural data reveals this as a false summit: the presence of organized beneficiaries (state governments, federal power centers) with direct regulatory interests contradicts the mountain classification. This is a reading that benefits identifiable institutional actors, not an immutable fact about text.
constraint_indexing:constraint_classification(collective_militia_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: PROGRESSIVE LEGAL MOVEMENT (PITON) — Organizations committed to gun control regulation (Brady Campaign, Giffords, many law schools) institutionally promote this reading. For these actors, the constraint now performs inertial function: the reading is institutionally maintained as orthodoxy in many legal institutions, but the actual leverage has degraded since the District of Columbia v. Heller (2008) Supreme Court decision, which rejected this reading and established an individual rights doctrine. The theater ratio is relatively low (0.35) for this perspective because the institutions claim to be doing serious constitutional interpretation, not mere ritual. But from the vantage of post-Heller doctrine, this reading's institutional presence is largely maintained through academic inertia and political commitment rather than doctrinal force. This is a degraded constraint that persists through institutional loyalty rather than judicial power.
constraint_indexing:constraint_classification(collective_militia_reading, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(collective_militia_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(collective_militia_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(collective_militia_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(collective_militia_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(collective_militia_reading, TR),
    TR >= 0.70.

:- end_tests(collective_militia_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): High. The reading grants state governments near-total regulatory discretion over individual firearm possession without constitutional constraint. This is extreme asymmetry — states can extract by prohibiting, regulating, licensing, registering, or taxing firearms at will. The extraction rises from 0.72 to 0.81 between 1791 and 1991 as states increasingly exercise this regulatory authority and federal courts (pre-Heller) deferred to it. Post-Heller (2008), extractiveness drops slightly to 0.78 because Heller reestablished an individual rights baseline that constrains but does not eliminate state regulatory authority. Suppression (0.82): Very high. Individual gun owners have no constitutional exit from state regulation. Their exit options are trapped (cannot claim constitutional protection) or constrained (can relocate to more permissive state, but this is costly). The suppression reflects the constitutional reading's mechanism: it denies the victim's claim not through external barriers but through textual interpretation. Theater ratio (0.35): Moderate-low. The collective reading makes a direct textual argument (prefatory clause determines operative clause meaning) without relying on procedural complexity or institutional theater. However, the reading's persistence post-Heller reflects institutional inertia rather than doctrinal force, suggesting theater has increased in the institutional maintenance (piton dynamics) even as the reading's core argument remains straightforward.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. State governments see coordination (Rope) — the reading legitimates their regulatory authority and clarifies federalism boundaries. The individual gun owner sees pure extraction (Snare) — the reading denies their constitutional claim entirely with maximum suppression. The firearms industry sees mixed dynamics (Tangled Rope) — they benefit from regulatory clarity but suffer from sweeping regulatory authority. The progressive legal movement sees inertial maintenance (Piton) — the reading is institutionally promoted despite post-Heller doctrinal degradation. The Supreme Court's analytical perspective (post-Heller) treats the reading as judicially foreclosed but acknowledges its historical plausibility. The false summit candidate is the analytical observer from a civilizational scope (Mountain perspective) — the reading can present itself as a necessary consequence of textual interpretation and linguistic logic, but the structural data (identifiable state beneficiaries with direct regulatory interests) reveals this as naturalization of a politically motivated interpretive choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies dramatically across perspectives. State governments as beneficiaries with arbitrage exit derive d ≈ 0.08 (full beneficiary), producing negative or minimal effective extraction. Individual gun owners as victims with trapped exit derive d ≈ 0.95 (full target), producing maximum experienced extraction through the sigmoid f(d) ≈ 1.42. The firearms industry as organized victims with constrained exit derives d ≈ 0.60 (moderate target), producing moderate-high experienced extraction. These directionality gaps create the perspectival divergence: the same constitutional reading produces different experienced extractiveness depending on the observer's structural position relative to the constraint. The analytical perspective uses canonical d ≈ 0.73 for the analytical power atom, producing moderate-high experienced extraction in the analyst's evaluation.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by explicit classification at 0.78 extractiveness: the collective reading IS a snare (not a rope misclassified as snare, not a tangled rope misclassified as snare). The extraction is genuine and asymmetric — state regulatory authority benefits states while denying individual claims. The coordination function is present but minimal: states might claim that militia regulation requires clarity about individual firearm access, but the coordination benefit flows overwhelmingly to the state. Under the classical mandatrophy test (whether high extraction is coordinating an essential problem or merely extracting), the collective reading fails the coordination gate: the problem being coordinated (state militia maintenance) does not require that individual self-defense claims be constitutionally foreclosed. Alternative architectures (e.g., individual rights with state regulatory authority, as in current doctrine post-Heller) solve the coordination problem without the extraction. Therefore, the extraction is mandatrophy-confirmed as genuine extraction rather than misclassified coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prefatory_clause_binding_force,
    'Does the prefatory clause (''well regulated Militia, being necessary'') logically bind the operative clause''s scope, or does it merely state a rationale that does not limit operative clause applicability?',
    'Comparative analysis of 18th-century constitutional drafting practices (preambles vs operative text in state constitutions, Federalist Papers interpretation); linguistic philosophy analysis of how prefatory clauses function in legal texts; historical evidence of founders'' intent regarding individual vs collective militia purpose.',
    'If prefatory clause is binding: collective reading is structurally sound, ε remains 0.78, classification remains Snare. If prefatory clause is merely rationale: individual rights reading becomes structurally viable, ε drops to 0.25, classification shifts to Rope or Mountain (coordination only). This omega resolves the kernel entirely — there is no third middle ground where both readings coexist.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(prefatory_clause_binding_force, empirical, 'Whether the prefatory clause logically binds the operative clause scope').

omega_variable(
    militia_definition_historical_scope,
    'In 18th-century constitutional context, did ''militia'' denote: (a) exclusively organized state military formations under state control, (b) able-bodied citizens generally capable of bearing arms, or (c) both meanings simultaneously with ambiguity resolved by context?',
    'Examination of militia acts pre-1791 (state and colonial level); Dictionary of the Scots Law and legal terminology of the period; contemporary state constitutional provisions using ''militia''; Federalist and Anti-Federalist writings discussing militia provisions.',
    'If (a) only state formations: collective reading is texturally sound. If (b) citizens generally: individual reading gains traction. If (c) ambiguous: both readings can claim historical warrant, shifting this from an empirical question to a normative interpretive choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_definition_historical_scope, empirical, 'Historical definition of militia in 18th-century constitutional context').

omega_variable(
    reading_stability_post_heller,
    'Is the collective reading theoretically viable as a constitutional interpretation after District of Columbia v. Heller (2008), or does Heller''s majority opinion logically foreclose it as a live judicial option in federal courts?',
    'Close reading of Heller majority opinion''s treatment of collective reading; analysis of subsequent Supreme Court precedent (McDonald, NYSRPA v Bruen); assessment of whether Heller created binding precedent on the prefatory-clause interpretation or left space for reinterpretation.',
    'If Heller forecloses: the reading persists as academic and political position but has degraded judicial force (piton status confirmed). If Heller leaves space: reading remains a live constitutional option. If Heller is itself vulnerable to reversal: reading''s institutional isolation may be temporary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_stability_post_heller, empirical, 'Whether Heller precedent viability after 2008').

omega_variable(
    interstate_regulatory_variation_sustainability,
    'Can a federalism regime based on near-total state regulatory discretion over individual firearm possession (per this reading) sustain itself as state laws diverge toward extreme variation (some states near-total prohibition, others near-total permission)?',
    'Historical analysis of how similar federalism regimes (contraception pre-Griswold, marriage law pre-Obergefell) handled extreme interstate variation; measurement of current state-to-state divergence in firearm regulation; analysis of federal enforcement capacity against sanctuary jurisdictions.',
    'If variation is sustainable: the snare structure holds indefinitely. If variation creates instability: federalism regime may collapse, forcing either federal preemption (collective reading becomes inoperable) or Supreme Court intervention (reading is judicially rejected).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interstate_regulatory_variation_sustainability, empirical, 'Sustainability of extreme interstate regulatory variation under this reading').

omega_variable(
    identity_lock_in_constitutional_reading,
    'To what extent is adherence to the collective reading bound up with institutional identity commitments (progressive legal establishment, state sovereignty advocates, gun control organizations) such that the reading persists through identity fusion rather than doctrinal force?',
    'Longitudinal analysis of law school curriculum emphasis on collective reading before and after Heller; tracking of constitutional scholars'' positions correlating with political/institutional affiliation; analysis of how many scholars maintain the reading despite acknowledging Heller''s textual difficulties.',
    'If primarily identity-locked: the reading''s institutional persistence is inertial (piton) and vulnerable to generational turnover or institutional realignment. If doctrinal force dominates: the reading has structural independence from political commitments.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_constitutional_reading, empirical, 'Degree of identity-lock in institutional adherence to collective reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(collective_militia_reading, 0, 217).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_1791, collective_militia_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(theater_1941, collective_militia_reading, theater_ratio, 150, 0.32).
narrative_ontology:measurement(theater_1991, collective_militia_reading, theater_ratio, 200, 0.38).
narrative_ontology:measurement(theater_2008_post_heller, collective_militia_reading, theater_ratio, 217, 0.35).

% Extraction over time
narrative_ontology:measurement(extractiveness_1791, collective_militia_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(extractiveness_1941, collective_militia_reading, base_extractiveness, 150, 0.76).
narrative_ontology:measurement(extractiveness_1991, collective_militia_reading, base_extractiveness, 200, 0.81).
narrative_ontology:measurement(extractiveness_2008_post_heller, collective_militia_reading, base_extractiveness, 217, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(collective_militia_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(collective_militia_reading, individual_rights_reading).
narrative_ontology:affects_constraint(collective_militia_reading, sophisticated_collective_reading).
narrative_ontology:affects_constraint(collective_militia_reading, federalism_militia_boundaries).

% DUAL FORMULATION NOTE:
% The collective_militia_reading is one constraint within a three-member kernel family (second_amendment_text). All three readings interpret the same constitutional text but produce different ε values and beneficiary/victim structures. The collective reading (ε=0.78, Snare) reflects high regulatory extraction. The individual_rights_reading (ε≈0.25, Mountain or Rope) reflects minimal extraction under individual protection doctrine. The sophisticated_collective_reading (ε≈0.50, Tangled Rope) reflects mixed coordination and extraction. These are not the same constraint viewed from different angles — they have structurally different ε values, beneficiary structures, and suppression mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
