% ============================================================================
% CONSTRAINT STORY: constitutional_authority_boundary__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_authority_boundary__judicial_supremacy_reading, []).

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
 *   constraint_id: constitutional_authority_boundary__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy in Constitutional Interpretation (Judicial Monopoly Reading)
 *   domain: constitutional_law/institutional_authority
 *
 * SUMMARY:
 *   The judicial supremacy reading of constitutional authority holds that the
 *   Constitution establishes the federal courts — and ultimately the Supreme
 *   Court — as the final, unchallengeable arbiters of all constitutional
 *   questions. Under this reading, courts possess authority to invalidate
 *   acts of the legislative and executive branches without remedy or override
 *   (except through the prohibitively costly amendment mechanism). This
 *   constraint story models ONE reading of the contested kernel of
 *   constitutional authority distribution. Sibling readings interpret the
 *   same constitutional text differently: the coordinate construction reading
 *   holds that all three branches are co-equal interpreters bound by their
 *   own oaths to constitutional fidelity, with no branch having final
 *   authority; the parliamentary primacy reading (less dominant in U.S.
 *   doctrine but extant in comparative constitutional law) holds that the
 *   legislature retains interpretive supremacy subject only to formal
 *   amendment. The judicial supremacy reading has been institutionally
 *   dominant in U.S. constitutional practice since Marbury v. Madison (1803),
 *   but it is not logically entailed by the constitutional text — the text is
 *   silent on which branch interprets the Constitution 'finally.' The reading
 *   creates a measurable constraint: extractiveness = 0.62 (moderate-high),
 *   suppression = 0.58 (moderate-high), theater_ratio = 0.48 (moderate). The
 *   constraint exhibits characteristics of Tangled Rope: it provides genuine
 *   coordination (prevents competing branches from offering irreconcilable
 *   constitutional interpretations), but it is enforced asymmetrically such
 *   that one branch (the judiciary) benefits from the interpretive monopoly
 *   rents while another branch (the legislature) bears the extraction cost of
 *   constrained policy space.
 *
 * KEY AGENTS:
 *   - Federal Judiciary: Primary beneficiary (institutional/arbitrage). Extracts interpretive monopoly rents — authority to declare what the Constitution 'really says' without peer review or override. Frames this extraction as constitutional duty and coordination necessity.
 *   - Legislative Branch: Primary victim (institutional/trapped). Constrained by veto power with no formal remedy except the prohibitively costly amendment procedure. Cannot enforce electoral mandates when courts declare them unconstitutional.
 *   - Voting Public/Citizens: Secondary victim (moderate/trapped). Electoral preferences channeled through legislation are subject to judicial invalidation. No direct appeal mechanism.
 *   - State Governments: Secondary victim (institutional/constrained). Subject to federal judicial override in federalism disputes but benefit from coordination function providing stable constitutional framework.
 *   - Constitutional Amendment Coalition: Organized actor (organized/constrained). Formal override mechanism exists but requires 2/3 supermajority — de facto insulation of judicial doctrine from normal amendment politics.
 *   - Judicial Supremacist Legal Scholars: Beneficiary (institutional/arbitrage). Frame supremacy as logical necessity and constitutional truth, naturalizing the institutional advantage.
 *   - Analytical Observer: Sees both functional coordination and institutional extraction; risk of naturalizing supremacy as natural law (false summit danger).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_authority_boundary__judicial_supremacy_reading, 0.62).
domain_priors:suppression_score(constitutional_authority_boundary__judicial_supremacy_reading, 0.58).
domain_priors:theater_ratio(constitutional_authority_boundary__judicial_supremacy_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_authority_boundary__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_authority_boundary__judicial_supremacy_reading, "Judicial Supremacy in Constitutional Interpretation (Judicial Monopoly Reading)").
narrative_ontology:topic_domain(constitutional_authority_boundary__judicial_supremacy_reading, "constitutional_law/institutional_authority").

domain_priors:requires_active_enforcement(constitutional_authority_boundary__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_authority_boundary__judicial_supremacy_reading, 'ksv7-m4n2-p9q1-r8x3').
narrative_ontology:cs_kernel_codification('ksv7-m4n2-p9q1-r8x3', fixed_text).
narrative_ontology:cs_authority_grounding('ksv7-m4n2-p9q1-r8x3', lineage).
narrative_ontology:cs_interpretation_layer_present('ksv7-m4n2-p9q1-r8x3').
narrative_ontology:cs_reading_relation('ksv7-m4n2-p9q1-r8x3', constitutional_authority_boundary__coordinate_construction_reading, coexists_with).
narrative_ontology:cs_reading_relation('ksv7-m4n2-p9q1-r8x3', constitutional_authority_boundary__parliamentary_primacy_reading, influences).
narrative_ontology:cs_axiom('ksv7-m4n2-p9q1-r8x3', foundational, courts_are_exclusive_constitutional_interpreters).
narrative_ontology:cs_axiom_status(courts_are_exclusive_constitutional_interpreters, holdable).
narrative_ontology:cs_axiom_grounding('ksv7-m4n2-p9q1-r8x3', courts_are_exclusive_constitutional_interpreters, conventional).
narrative_ontology:cs_axiom('ksv7-m4n2-p9q1-r8x3', foundational, legislative_override_constitutionally_forbidden).
narrative_ontology:cs_axiom_status(legislative_override_constitutionally_forbidden, holdable).
narrative_ontology:cs_axiom_grounding('ksv7-m4n2-p9q1-r8x3', legislative_override_constitutionally_forbidden, deontological).
narrative_ontology:cs_reference_frame('ksv7-m4n2-p9q1-r8x3', judicial_supremacy_established).
narrative_ontology:cs_drift_state('ksv7-m4n2-p9q1-r8x3', contemporary_institutional_challenge, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ksv7-m4n2-p9q1-r8x3', '').
narrative_ontology:cs_kernel_id(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_authority_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__judicial_supremacy_reading, federal_judiciary).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__judicial_supremacy_reading, judicial_supremacist_legal_scholars).
narrative_ontology:constraint_victim(constitutional_authority_boundary__judicial_supremacy_reading, legislative_branch).
narrative_ontology:constraint_victim(constitutional_authority_boundary__judicial_supremacy_reading, popular_sovereignty_mechanisms).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LEGISLATURE (SNARE) — Trapped by the supremacy doctrine. Cannot override constitutional interpretation through ordinary legislation. Faces veto power with no remedy. Extraction flows unidirectionally from legislative authority to judicial authority. Theater is low here — the veto is functional, not performative.
constraint_indexing:constraint_classification(constitutional_authority_boundary__judicial_supremacy_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: VOTING PUBLIC (SNARE) — Citizens cannot enforce their electoral preferences through legislation when courts invalidate them as 'unconstitutional' under judicially-imposed doctrine. High suppression: no formal appeal mechanism, no legislative override, no constitutional amendment for minor claims (amendment is prohibitively costly). Extraction is the constraint on democratic will-formation.
constraint_indexing:constraint_classification(constitutional_authority_boundary__judicial_supremacy_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: FEDERAL JUDICIARY (ROPE) — Experiences the supremacy doctrine as pure coordination: the constraint solves the problem of how to adjudicate conflicting interpretations of the constitutional text. Judiciary has exit via arbitrage — can reinterpret doctrine without formal override. Extracts legitimacy premium and policy veto power but frames this as constitutional duty, not extraction. Benefits from interpretive monopoly rents (unilateral authority to declare what the Constitution 'really says').
constraint_indexing:constraint_classification(constitutional_authority_boundary__judicial_supremacy_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE GOVERNMENTS (TANGLED ROPE) — Constrained by federal judicial override but also benefit from the coordination function: settled constitutional doctrine provides stable boundaries for federalism disputes. Subordinate but not entirely helpless — can navigate doctrine strategically, engage in constitutional reconstruction through state courts and administrative practice. Moderate extraction: veto power without remedy for major doctrinal shifts, but also genuine coordination benefit from stable constitutional framework.
constraint_indexing:constraint_classification(constitutional_authority_boundary__judicial_supremacy_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL AMENDMENT COALITION (TANGLED ROPE) — Organized actors (scholars, reform movements, political coalitions) see the supremacy doctrine as a coordination mechanism with asymmetric extraction. Amendment provides a formal override mechanism but at prohibitive cost (2/3 supermajority requirement). Extraction is the supermajority threshold itself — ensures judicial veto persists for policies with merely majoritarian support. Benefits from coordination (clear hierarchy); bears extraction cost (supermajority lock-in).
constraint_indexing:constraint_classification(constitutional_authority_boundary__judicial_supremacy_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: JUDICIAL REVIEW AS INSTITUTIONAL RITUAL (PITON) — From a civilizational view, the supremacy doctrine as originally conceived (Marbury v. Madison, 1803) performed coordination: it prevented multiple branches from offering competing constitutional interpretations. But the primary function — preventing constitutional chaos — has been absorbed into institutional practice. What remains is largely performative: the appearance of constitutional law as discovered truth rather than constructed doctrine. Theater ratio is moderate (0.48) because actual policy consequences follow from rulings, but the mechanism of legitimacy has degraded to claiming interpretive monopoly rather than demonstrating unique authority.
constraint_indexing:constraint_classification(constitutional_authority_boundary__judicial_supremacy_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER — COORDINATION VIEW (ROPE) — From the analytical/civilizational position, the supremacy doctrine solves a real coordination problem: in a written-constitution system, there must be some mechanism to adjudicate competing interpretations and resolve constitutional disputes. The Court's monopoly on final interpretation prevents constitutional chaos from multiple branches offering irreconcilable readings. Under this view, suppression and extraction are coordination costs, not oppression. However, this perspective risks naturalizing the specificity of the judicial supremacy solution — other constitutional democracies use parliamentary supremacy or coordinate interpretation models with equal success. Rope classification holds only if we accept that ONLY judicial supremacy solves the coordination problem; if alternatives work, the classification slides toward tangled_rope.
constraint_indexing:constraint_classification(constitutional_authority_boundary__judicial_supremacy_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_authority_boundary__judicial_supremacy_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(constitutional_authority_boundary__judicial_supremacy_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(constitutional_authority_boundary__judicial_supremacy_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_authority_boundary__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(constitutional_authority_boundary__judicial_supremacy_reading, TR),
    TR >= 0.70.

:- end_tests(constitutional_authority_boundary__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): The judicial supremacy reading generates moderate-high extraction because the judiciary captures unilateral authority to interpret the Constitution and invalidate legislation without remedy. This is a significant policy veto power that benefits the judiciary (career authority, institutional prestige, ability to shape law unilaterally) while constraining other branches. However, extractiveness does not reach the snare level (0.66+) because: (1) genuine coordination function exists — a single authoritative interpreter prevents constitutional chaos from competing interpretations, and (2) the legislature retains the formal override mechanism via amendment, albeit at prohibitive cost. The 0.62 value reflects that this reading is extractive (it advantages one branch) but not purely extractive (some coordination function is real). Temporal rise from 0.45 (1803) to 0.68 (1970) reflects intensification: as the Court has exercised its veto power more frequently and across broader domains (civil rights, executive authority, regulatory scope), the extraction becomes more visible and less deniable as mere coordination cost. Suppression (0.58): Moderate-high. The constraint operates through institutional design rather than coercion, but it is embedded in constitutional law and enforcement machinery that makes escape difficult. The legislature cannot simply 'opt out' of judicial review; citizens cannot appeal veto power to a higher authority. Formal exit mechanism (amendment) exists but requires 2/3 supermajority — de facto insulation. Theater ratio (0.48): Moderate. Judicial review is functionally consequential — laws are actually invalidated, with real policy effects. However, the mechanism of legitimacy has shifted: originally, Marbury justified review as logical necessity (to prevent constitutional chaos); currently, the Court claims interpretive supremacy as inherent constitutional right, asserting rather than justifying. The theater has risen from 0.35 (Marbury era, when logical necessity was the claimed warrant) to 0.48 (modern era, when supremacy is asserted as constitutional duty without the same logical force).
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a striking perspectival divide. From the judiciary's position (institutional/arbitrage), the supremacy doctrine is pure coordination — the constraint solves the real problem of preventing constitutional cacophony from multiple branches offering competing readings. This agent experiences the doctrine as Rope: enabling interpretation, providing authority to adjudicate disputes, generating the legitimacy to settle constitutional questions authoritatively. From the legislature's position (institutional/trapped), the same doctrine is pure extraction — a veto on lawmaking with no appeal. The legislature cannot override judicial interpretation except through the supermajority amendment process. This agent experiences the doctrine as Snare: constrained, powerless, subject to unilateral veto by another branch. From the analytical/civilizational position (analytical/analytical), the supremacy doctrine risks appearing as natural law — as if there is no alternative way to organize constitutional authority. But comparative law reveals this risk: parliamentary supremacy (UK, Canada pre-1982) and coordinate interpretation (EU, Switzerland) solve the constitutional coordination problem differently, demonstrating that supremacy is one institutional choice, not a natural law. The Piton perspective reveals that the primary function (preventing constitutional chaos) has been absorbed into institutional practice; what remains is increasingly performative — the assertion of interpretive supremacy without the logical justification that originally motivated Marbury. The Amendment Coalition perspective reveals the supermajority lock-in: the formal override mechanism is so expensive that it functions as insulation of judicial doctrine rather than remedy.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value derives from the agent's structural position relative to the extraction flow. The judiciary (beneficiary/arbitrage) experiences low d because it benefits from the supremacy doctrine and can exit through reinterpretation — it has the power to declare what the Constitution 'really says,' shifting doctrine without formal override. This produces negative or low f(d), meaning the judiciary experiences the constraint as less extractive than raw ε suggests. The legislature (victim/trapped) experiences high d because it bears the extraction cost (constrained lawmaking space) and cannot exit without supermajority amendment — it is trapped by the supremacy doctrine without remedy. This produces high f(d), meaning the legislature experiences the constraint as more extractive than raw ε suggests. The voting public (victim/trapped) has similar high d and high f(d) — electoral preferences are subject to judicial veto. The amendment coalition (victim/constrained at organizational level) experiences moderate d because the formal override mechanism exists but at prohibitive cost; they are not entirely trapped but face supermajority barriers. The engine derives d automatically from these structural declarations (beneficiary/arbitrage, victim/trapped, etc.); the perspectival gap in chi values emerges from the different d values, not from observer bias.
 *
 * MANDATROPHY ANALYSIS:
 *   The judicial supremacy reading exhibits moderate mandatrophy: the constraint is classified as Tangled Rope, not Snare, because the coordination function is genuine (prevents constitutional chaos), even though extraction is visible. The mandatrophy is resolved by recognizing that this reading IS one institutional choice among alternatives. The sibling readings (coordinate construction, parliamentary primacy) represent live alternatives that solve the same coordination problem differently. To verify that the Tangled Rope classification is correct rather than mislabeling extraction as coordination: (1) test whether alternative readings (parliamentary supremacy, coordinate interpretation) can functionally solve the coordination problem. They can — no branch has final authority in EU law, yet constitutional disputes are resolved. Therefore, supremacy is not necessary for coordination. (2) This reclassifies supremacy as primarily extractive (advantages judiciary) with coordination as secondary effect. However, within the U.S. institutional context and the supremacy reading's own framework, coordination IS primary — the reading sincerely justifies supremacy as coordination necessity. Mandatrophy is resolved: Tangled Rope is correct for this reading's internal perspective (genuinely mixes coordination and extraction), but the Snare perspectives (legislature, voting public) and the analytical perspective that recognizes alternatives are also correct. The six perspectives accurately capture the constraint's structural ambiguity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    supremacy_versus_coordinate_equivalence,
    'Does judicial supremacy uniquely solve the constitutional interpretation coordination problem, or do parliamentary supremacy and coordinate construction models solve it equally well?',
    'Comparative constitutional analysis: Canada (parliamentary supremacy with Charter override), EU (court-led but with member-state veto), UK (common-law supremacy with parliament override), state constitutions with legislative amendment mechanisms. Measurement: success at preventing constitutional chaos, legitimacy maintenance, adaptation to social change.',
    'If supremacy is unique solution: coordination pure function, classification toward Rope. If alternatives work: supremacy is one institutional choice among several, extraction becomes visible as policy veto divorced from coordination necessity, classification toward Snare or Tangled Rope for more perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(supremacy_versus_coordinate_equivalence, empirical, 'Whether judicial supremacy uniquely solves constitutional coordination or alternatives are functionally equivalent').

omega_variable(
    judicial_capacity_degradation,
    'Has the Court''s capacity to genuinely adjudicate constitutional meaning degraded relative to its claimed supremacy authority, enabling extraction under the cover of coordinate interpretation?',
    'Historical analysis: (1) frequency of overruled precedents, (2) doctrinal instability (reversals of settled doctrine within 10-20 year windows), (3) justice opinion writing as pure power play vs. doctrinal reasoning, (4) specialist/informed public perception of Court legitimacy vs. baseline institutional authority. Measurement period: 1970s-present.',
    'If degradation is severe: supremacy doctrine persists through institutional inertia (Piton), theater ratio rises, extractiveness interpretation shifts from functional coordination to power monopoly extraction. If capacity stable: coordination hypothesis holds, Rope perspective gains support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_capacity_degradation, empirical, 'Whether judicial capacity for genuine constitutional adjudication has degraded while supremacy claims persist').

omega_variable(
    alternative_reading_logical_structure,
    'Do the sibling readings (coordinate_construction, parliamentary_primacy) logically foreclose the judicial supremacy reading, or do they coexist as live institutional choices within the constitutional framework?',
    'Jurisprudential analysis: identify the core axioms each reading depends on. If the axioms are mutually exclusive within a single framework (e.g., ''the Constitution assigns final interpretive authority to the judiciary'' vs. ''the Constitution does not assign authority, it distributes it''), mark as forecloses. If each reading can be true within its own institutional context or party''s commitment structure, mark as coexists_with.',
    'If forecloses: only one reading can legitimately claim to represent the constitution''s meaning; the others are formally defeated. Reclassify as foreclosed within the commitment system analysis. If coexists_with: all three readings remain live; the constraint represents an active institutional dispute unresolved by textual analysis alone.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_reading_logical_structure, conceptual, 'Logical relationship between judicial supremacy and alternative constitutional readings').

omega_variable(
    amendment_cost_as_intended_constraint,
    'Is the high cost of constitutional amendment (2/3 supermajority) an intentional structural check on majoritarian override of judicial doctrine, or a side effect of the amendment procedure''s design for stability?',
    'Framers'' intent analysis: examine constitutional convention debates and early commentary on why 2/3 was chosen. Distinguish: (1) intended to protect enumerated rights from simple majorities (yes — framers said this), (2) intended specifically to insulate judicial interpretation from override (unclear — framers did not address, amendment was not used to override the Court for 150+ years). If (1) only: amendment cost is independent of judicial supremacy. If (2): amendment cost is a structural partner to supremacy, amplifying extraction.',
    'If (2): extraction is compounded — judicial veto + supermajority lock-in. Extractiveness rises from 0.62 to 0.70+, requiring mandatrophy analysis. If (1) only: extractiveness remains 0.62, amendment cost is separate constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_cost_as_intended_constraint, empirical, 'Whether amendment supermajority is intentionally designed to insulate judicial doctrine or independent institutional choice').

omega_variable(
    committer_frame_kernel_identity,
    'Is the constitutional text (''final, unchallengeable arbiter'') actually a stable kernel of judicial supremacy, or is this reading''s interpretation of the text one contested reading among the sibling readings?',
    'Textual analysis of Article III and historical dispute. The text says ''the judicial power shall extend to all cases arising under this Constitution'' and says nothing about supremacy. Marbury v. Madison (1803) inferred supremacy from logical necessity (coordinate interpretation leads to chaos). The text is ambiguous; supremacy is not stated. Therefore, the ''kernel'' is the ambiguous constitutional text on the judiciary''s role; this reading interprets it as supremacy; sibling readings interpret it as coordinate or parliamentary. The kernel is not THE SUPREMACY but the BOUNDARY QUESTION: what authority can interpret the Constitution authoritatively?',
    'Classification confirmed: this is ONE reading of a contested kernel. The constraint story accurately models the reading, not the kernel. Sibling stories will model coordinate and parliamentary readings of the same kernel. FSM alert: this constraint may be misclassified if the analytical perspective naturalizes supremacy as inherent law rather than as one institutional choice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_kernel_identity, conceptual, 'Kernel identity: contested question of constitutional authority vs. supremacy doctrine as stated fact').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_authority_boundary__judicial_supremacy_reading, 0, 220).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jud_sup_theater_1803, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(jud_sup_theater_1860, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 60, 0.4).
narrative_ontology:measurement(jud_sup_theater_1920, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 120, 0.45).
narrative_ontology:measurement(jud_sup_theater_1970, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 170, 0.48).

% Extraction over time
narrative_ontology:measurement(jud_sup_extract_1803, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(jud_sup_extract_1860, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 60, 0.58).
narrative_ontology:measurement(jud_sup_extract_1920, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 120, 0.62).
narrative_ontology:measurement(jud_sup_extract_1970, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 170, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(jud_sup_suppress_1803, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(jud_sup_suppress_1920, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 120, 0.56).
narrative_ontology:measurement(jud_sup_suppress_1970, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 170, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_authority_boundary__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_authority_boundary__coordinate_construction_reading).
narrative_ontology:affects_constraint(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_authority_boundary__parliamentary_primacy_reading).
narrative_ontology:affects_constraint(constitutional_authority_boundary__judicial_supremacy_reading, amendment_supermajority_lock_in).

% DUAL FORMULATION NOTE:
% The constitutional authority boundary constraint family consists of three kernel readings: judicial supremacy, coordinate construction, and parliamentary primacy. Each instantiates a different constraint with distinct extractiveness values, suppression mechanisms, and beneficiary/victim structures. All three readings interpret the same constitutional kernel (the text on the judiciary's role) but arrive at different institutional arrangements. The network structure captures: supremacy affects the amendment mechanism (supermajority lock-in is enabled by supremacy doctrine's institutional dominance) and affects both sibling readings (each is a structural alternative to supremacy that would decompose the same institutional arrangement differently). Decomposition follows ε-invariance: if the observable changed from 'who can finally interpret the Constitution' to 'how do competing interpretations get resolved,' we would be measuring a different constraint. Each reading is a distinct story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_authority_boundary__judicial_supremacy_reading, institutional, 0.15).
constraint_indexing:directionality_override(constitutional_authority_boundary__judicial_supremacy_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
