% ============================================================================
% CONSTRAINT STORY: fifth_republic_constitution__hyper_presidential_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fifth_republic_constitution__hyper_presidential_reading, []).

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
 *   constraint_id: fifth_republic_constitution__hyper_presidential_reading
 *   human_readable: Fifth Republic Hyper-Presidential Reading: Direct National Sovereignty with Minimal Legislative Constraint
 *   domain: constitutional_law/political_systems
 *
 * SUMMARY:
 *   This constraint story instantiates ONE reading of the Fifth Republic
 *   Constitution: the hyper-presidential interpretation where the president
 *   directly embodies national sovereignty with minimal legislative
 *   constraint. This is not a claim about constitutional text alone, but
 *   about how that text is read and institutionalized through practice. The
 *   Fifth Republic Constitution (1958) is an ambiguous kernel with multiple
 *   competing readings — the hyper-presidential reading emphasizes Article 5
 *   (the president's role as guardian of national independence and
 *   constitutional continuity), direct presidential election, executive
 *   decree authority, and emergency powers as the constitutional core,
 *   treating the legislature as a secondary coordinating institution. From
 *   this reading, the president is the supreme authority constrained only by
 *   direct democracy (elections, referenda) and formal constitutional
 *   amendment. The legislature is positioned as subordinate: it may
 *   legislate, but the executive retains extensive parallel authority and
 *   override mechanisms (49.3, Article 16, decree power, dissolution). This
 *   reading has progressively strengthened over the Fifth Republic's history
 *   through jurisprudential drift, institutional practice, and electoral
 *   realignment. The measurement trajectory shows extractiveness rising from
 *   0.45 (immediate post-1958 period with stronger legislative prerogative)
 *   to 0.68 (contemporary executive dominance). Theater ratio and suppression
 *   requirement have also risen, indicating that constitutional review has
 *   been increasingly sidelined and legislative alternatives increasingly
 *   foreclosed.
 *
 * KEY AGENTS:
 *   - The Presidency (as institution and incumbent): Primary beneficiary (institutional/arbitrage) — holds directly elected national mandate, controls executive branch, appoints government, invokes Article 49.3 and Article 16, dissolves National Assembly. Maximum arbitrage: can restructure the political game through elections or emergency powers.
 *   - National Assembly / Legislature: Primary victim (moderate to powerless, depending on electoral alignment) — formally sovereign in legislative domain but increasingly constrained by executive mechanisms and constitutional design. Trapped when opposition-led; constrained even with presidential coalition.
 *   - Opposition Parties: Secondary victim (powerless/trapped) — cannot prevent presidential action even when holding assembly majority in some domains (executive appointments, foreign policy, emergency powers). Formal alternatives (no-confidence, budget rejection) carry high institutional costs.
 *   - Constitutional Court (Conseil Constitutionnel): Institutional actor (institutional/arbitrage but with degraded function) — formally tasked with constraining executive power but progressively limited to post-hoc review; many emergency powers removed from prior restraint. Piton: maintains constitutional theater without effective constraint.
 *   - Electorate: Moderate power (powerful/mobile) — directly elects president with vast powers but remains locked into five-year cycles; can change presidential choice but cannot meaningfully constrain between elections.
 *   - Analytical Observer: Sees the constraint as naturalized (mountain) but structural data reveals false summit — the 'modernity requires executive power' framing masks a constitutional choice.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fifth_republic_constitution__hyper_presidential_reading, 0.68).
domain_priors:suppression_score(fifth_republic_constitution__hyper_presidential_reading, 0.65).
domain_priors:theater_ratio(fifth_republic_constitution__hyper_presidential_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fifth_republic_constitution__hyper_presidential_reading, snare).
narrative_ontology:human_readable(fifth_republic_constitution__hyper_presidential_reading, "Fifth Republic Hyper-Presidential Reading: Direct National Sovereignty with Minimal Legislative Constraint").
narrative_ontology:topic_domain(fifth_republic_constitution__hyper_presidential_reading, "constitutional_law/political_systems").

domain_priors:requires_active_enforcement(fifth_republic_constitution__hyper_presidential_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fifth_republic_constitution__hyper_presidential_reading, '401cc995-7770-4bcb-9254-d0568a04fece').
narrative_ontology:cs_kernel_codification('401cc995-7770-4bcb-9254-d0568a04fece', formalized).
narrative_ontology:cs_authority_grounding('401cc995-7770-4bcb-9254-d0568a04fece', lineage).
narrative_ontology:cs_interpretation_layer_present('401cc995-7770-4bcb-9254-d0568a04fece').
narrative_ontology:cs_reading_relation('401cc995-7770-4bcb-9254-d0568a04fece', fifth_republic_constitution__parliamentary_constraint_reading, coexists_with).
narrative_ontology:cs_reading_relation('401cc995-7770-4bcb-9254-d0568a04fece', fifth_republic_constitution__cohabitation_equilibrium_reading, influences).
narrative_ontology:cs_axiom('401cc995-7770-4bcb-9254-d0568a04fece', foundational, direct_national_sovereignty_through_presidency).
narrative_ontology:cs_axiom_status(direct_national_sovereignty_through_presidency, holdable).
narrative_ontology:cs_axiom_grounding('401cc995-7770-4bcb-9254-d0568a04fece', direct_national_sovereignty_through_presidency, deontological).
narrative_ontology:cs_axiom('401cc995-7770-4bcb-9254-d0568a04fece', secondary, executive_primacy_in_constitutional_hierarchy).
narrative_ontology:cs_axiom_status(executive_primacy_in_constitutional_hierarchy, holdable).
narrative_ontology:cs_axiom_grounding('401cc995-7770-4bcb-9254-d0568a04fece', executive_primacy_in_constitutional_hierarchy, instrumental).
narrative_ontology:cs_reference_frame('401cc995-7770-4bcb-9254-d0568a04fece', fifth_republic_direct_executive_mandate).
narrative_ontology:cs_drift_state('401cc995-7770-4bcb-9254-d0568a04fece', contemporary_2020s, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('401cc995-7770-4bcb-9254-d0568a04fece', '').
narrative_ontology:cs_kernel_id(fifth_republic_constitution__hyper_presidential_reading, fifth_republic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__hyper_presidential_reading, presidency_as_institution).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__hyper_presidential_reading, incumbent_president).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, legislature).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, constitutional_constraint_regime).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, opposition_parties).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OPPOSITION IN NATIONAL ASSEMBLY (SNARE) — Structurally trapped. Even with majority, executive mechanisms (Article 49.3 confidence votes, Article 16 emergency powers, executive decree authority) reduce legislative capacity to block presidential will. Opposition cannot exit the system without abandoning their institutional position. Suppression is maximal — legal alternatives are foreclosed by constitutional design.
constraint_indexing:constraint_classification(fifth_republic_constitution__hyper_presidential_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LEGISLATURE WITHOUT EXECUTIVE MAJORITY (SNARE) — Constrained extraction. When the National Assembly does not contain a presidential coalition majority, the legislature retains formal legislative authority but experiences extraction through executive workarounds: Article 49.3 (government declares confidence in legislation without vote), Article 16 (emergency decrees), executive ordering, budgetary discretion. The legislature sees itself as victim of constitutional design — formally sovereign but practically subordinated. High suppression: formal alternatives (no-confidence votes, budget rejection) carry institutional costs.
constraint_indexing:constraint_classification(fifth_republic_constitution__hyper_presidential_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE PRESIDENCY — HYPER-PRESIDENTIAL READING (ROPE) — Experiences the constraint as pure coordination. The presidential reading treats direct national sovereignty as the core function: the president embodies the will of the nation (Article 5), holds executive power, appoints the government, can dissolve the National Assembly, invokes emergency powers (Article 16), and uses Article 49.3 as a coordination mechanism to align government and legislature. From this perspective, legislative resistance is a coordination problem to be solved through democratic plebiscites (dissolution, new elections) or constitutional amendments — not an external constraint on presidential power. This reading has maximal arbitrage: the presidency can always restructure the game (dissolve assembly, call new elections, invoke emergency powers).
constraint_indexing:constraint_classification(fifth_republic_constitution__hyper_presidential_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THE ELECTORATE — GENERATIONAL VIEW (TANGLED ROPE) — Sees the constraint as genuine coordination with asymmetric extraction. The hyper-presidential reading distributes power highly asymmetrically: the electorate elects a president with vast powers once every five years, but between elections retains only indirect influence (through deputies, opinion pressure). At the generational time horizon, the electorate experiences both coordination benefits (the president can mobilize national will in crises, act quickly) and extraction costs (limited accountability between elections, presidential agenda-setting dominance). Mobile exit option: voters can change their choice every five years, but are locked into five-year cycles. Suppression moderate: constitutional amendments are technically possible but require supermajorities.
constraint_indexing:constraint_classification(fifth_republic_constitution__hyper_presidential_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL COURT / CONSEIL CONSTITUTIONNEL (PITON VIEW) — The Constitutional Court is formally tasked with constraining executive power through constitutional review, but the hyper-presidential reading has progressively narrowed the Court's practical capacity. Many executive powers (Article 16 emergency decrees, Article 49.3 votes of confidence without parliamentary votes) have been removed from prior restraint review or delegitimized presidential challenge through jurisprudential doctrines of political questions. The Court's review function persists (piton: institutional inertia) but has been substantially hollowed (theater ratio rises as review becomes largely post-hoc and reversible). The Court maintains constitutional theater without effective constraint.
constraint_indexing:constraint_classification(fifth_republic_constitution__hyper_presidential_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN READING) — From a civilizational/universal perspective, the hyper-presidential reading appears to naturalize executive dominance as an inherent feature of how modern nation-states must organize themselves: large, complex, rapid-response societies require executive action capacity; legislatures are too slow and fragmented for crisis response; therefore, presidential power is a structural necessity of modernity. This perspective sees the constraint as a mountain — an immutable feature of how modern states must function. However, the structural data (beneficiaries identified, victims trapped, active enforcement required) contradicts the mountain classification. This is a false summit: the 'modernity requires executive power' framing naturalizes what is actually a constitutional choice benefiting the presidency and harming legislative constraint.
constraint_indexing:constraint_classification(fifth_republic_constitution__hyper_presidential_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fifth_republic_constitution__hyper_presidential_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fifth_republic_constitution__hyper_presidential_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fifth_republic_constitution__hyper_presidential_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(fifth_republic_constitution__hyper_presidential_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(fifth_republic_constitution__hyper_presidential_reading, TR),
    TR >= 0.70.

:- end_tests(fifth_republic_constitution__hyper_presidential_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The hyper-presidential reading concentrates authority in the presidency through multiple mechanisms: direct election creates a mandate to act; Article 5 positions the president as guardian of the nation; Article 49.3 allows governments to bypass legislative majorities on key votes; Article 16 permits emergency decrees without legislative constraint; executive decree authority is broad; the president controls appointments, foreign policy, and defense. The legislature is reduced to a secondary forum for coordination and ratification. Extractiveness measures how much the constitutional design privileges the executive over legislative constraint — the value 0.68 reflects that the presidency enjoys substantial unilateral authority (>0.66 threshold for snare). Suppression (0.65): High. The legislature faces suppression through: constitutional design (limited amendment power, constrained override mechanisms), electoral cycles (president elected separately, may control different coalitions than legislature), institutional inertia (formal alternatives like no-confidence votes or budget rejection are available but carry high reputational/political costs), and jurisprudential doctrine (Constitutional Court has limited review scope for emergency and decree powers). Theater ratio (0.48): Moderate. The hyper-presidential reading produces less theater than the piton perspective's degraded review would suggest, because the reading does not claim that constitutional review is performative — it claims that the constitution itself, properly read, authorizes executive dominance. The theater reflects the gap between formal legislative prerogative (legislative authority in Articles 34-48) and actual executive capacity to act unilaterally (through 49.3, Article 16, decrees, dissolution). Theater is not maximal because the constitution provides genuine legislative authority; theater is significant because that authority is substantially circumvented.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between the presidential view (Rope: coordination mechanism to align executive-legislative action) and the legislative/opposition view (Snare: extraction mechanism that subordinates legislature to executive will) is a fundamental feature of this reading. The president sees the constraint as solution to the coordination problem of executive-legislative conflict; the legislature sees it as the problem. The electoral/generational perspective sees both coordination and extraction (Tangled Rope): the president can mobilize national will rapidly, but dominates agenda-setting between elections. The Constitutional Court's perspective (Piton) reveals that formal constraint mechanisms exist but have been institutionally degraded — the theater of constitutional review persists without functional constraint. The analytical observer risks naturalizing this asymmetry as an immutable feature of modernity (Mountain), but the structural data reveals it as a constitutional choice benefiting identifiable agents (the presidency) at the cost of others (the legislature, opposition parties). The perspectival gap is not a disagreement about facts, but a disagreement about whether the constraint is inherent to the constitutional design (presidential view) or a drift away from the original constraint regime (parliamentary reading, documented in sibling constraint).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values derive from the agent's structural position in this reading. The president benefits from the constraint and has maximum arbitrage — can dissolve the assembly, invoke emergency powers, call referenda — so d ≈ 0.05-0.20 (full beneficiary). The legislature is victimized by the constraint and has constrained or trapped exit, so d ≈ 0.75-0.95 (full target). Opposition parties are trapped when without majority, so d ≈ 0.90 (near-total target). The electorate has mobile exit (can change presidents every five years) but is locked into the constitutional cycle, so d ≈ 0.60 (moderate target). The Constitutional Court has arbitrage capacity (can limit or extend review scope) but is increasingly subordinated, so d ≈ 0.40 (mixed). The analytical observer is positioned at d ≈ 0.72 (analytical standard). These directionalities feed the sigmoid f(d) to produce experienced extractiveness chi for each perspective, generating perspectival variation while maintaining structural consistency.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is RESOLVED. The hyper-presidential reading explicitly accounts for and justifies asymmetric extraction: the president benefits because the constitution is designed to concentrate executive authority in a directly elected leader; the legislature is victimized because it is subordinated by constitutional structure and executive mechanisms. This is not a case of misclassified coordination — the reading acknowledges the extraction explicitly. The mandatrophy resolution route is through the kernel/reading distinction: this reading IS a legitimate interpretation of the constitutional text, but it is ONE READING among others (parliamentary reading, cohabitation reading). The engine's classification of this reading as Snare from most perspectives is not a paradox — it is the correct classification for THIS reading's core claim. A different reading (parliamentary constraint) would produce different classifications reflecting different constitutional priorities.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_49_3_legitimacy,
    'Does Article 49.3 represent a legitimate coordination mechanism for aligning executive and legislative agendas, or a mechanism for extracting legislative authority by circumventing majority requirements?',
    'Comparative constitutional analysis: frequency of 49.3 invocation vs parliamentary motion voting; analysis of legislation passed via 49.3 that would fail majority vote; cross-national comparison with executive decree mechanisms in other democracies',
    'If coordination: hyper-presidential reading remains Rope from presidential perspective, legislature remains constrained but benefiting. If extraction: legislature reclassifies to pure Snare; presidential constraint weakens significantly; democratic accountability gap widens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_49_3_legitimacy, conceptual, 'Whether Article 49.3 is coordination or extraction mechanism').

omega_variable(
    article_16_emergency_power_scope,
    'Does Article 16 emergency authority represent an inherent necessity for executive action in crisis, or a mechanism for bypassing legislative constraint during periods of declared emergency whose boundaries are politically manipulable?',
    'Historical analysis: frequency, duration, scope of Article 16 invocations; comparison of emergency declarations to objective crisis severity; analysis of legislation enacted under Article 16 that persists post-emergency; examination of whether emergency termination is endogenous (genuine crisis resolution) or exogenous (political pressure)',
    'If necessity: mountain perspective gains credibility; extractiveness may be recalibrated downward. If manipulable: confirms snare classification; extractiveness confirmed at high levels; suppression mechanism documented.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_16_emergency_power_scope, empirical, 'Scope and reversibility of Article 16 emergency authority').

omega_variable(
    legislative_cohabitation_structural_possibility,
    'Is cohabitation (divided government with opposition-led legislature) a structurally stable equilibrium, or a temporary deviation from hyper-presidential equilibrium that resolves toward executive dominance through electoral realignment or constitutional amendment?',
    'Longitudinal analysis of cohabitation periods: duration, legislative effectiveness, electoral outcomes; comparison with other semi-presidential systems; analysis of constitutional amendments or institutional reforms during cohabitation periods',
    'If stable equilibrium: hyper-presidential reading is contingent on electoral alignment; legislature retains real constraint capacity under cohabitation. If temporary deviation: confirms snare reading; suggests presidential power asymmetry is structural, not circumstantial.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legislative_cohabitation_structural_possibility, empirical, 'Whether cohabitation is stable or transitional in Fifth Republic structure').

omega_variable(
    democratic_constraint_regime_reading_distinction,
    'Is the victim here the ''legislature as institution'' or the ''regime of constitutional constraints on executive power'' — i.e., is this a story about executive-legislative institutional balance, or about the broader constitutional architecture''s capacity to constrain executive discretion?',
    'Analytical: clarify whether the extractiveness metric tracks the legislature''s formal authority loss (institutional metric) or the regime''s constraint capacity loss (systemic metric). Different metrics produce different omegas and different downstream classification consequences.',
    'If institutional: focuses on legislative capacity, executive-legislative balance, cohabitation dynamics. If systemic: expands analysis to constitutional amendment authority, judicial review, emergency power scope, extraconstitutional political pressure. Framing affects which agents classify as victims and which omega variables matter most.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(democratic_constraint_regime_reading_distinction, conceptual, 'Whether victims are legislature qua institution or democratic constraint regime').

omega_variable(
    kernel_reading_ambiguity,
    'Which reading is constitutionally primary: the hyper-presidential reading (national sovereignty concentrated in directly elected president) or the parliamentary reading (legislature as supreme expression of national will constrained by constitutional rule of law)?',
    'Textual analysis of Constitution of 1958: examine Preamble, Articles 1-6 (constitutional principles), Article 5 (presidential role), Articles 24-48 (legislative role), Articles 52-95 (presidential powers). Jurisprudential analysis: Constitutional Court evolution from Fourth Republic jurisprudence. Historical analysis: de Gaulle''s intended constitution vs. subsequent jurisprudential drift.',
    'If hyper-presidential is primary: this reading''s axioms (direct national sovereignty, executive primacy) are foundational; parliamentary reading coexists as subordinate reading. If parliamentary is primary: hyper-presidential reading is drift or interpretation-layer overflow; parliamentary constraint reading represents the kernel''s original commitment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Constitutional primacy of hyper-presidential vs parliamentary reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fifth_republic_constitution__hyper_presidential_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fr5hyper_tr_t0, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(fr5hyper_tr_t15, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 15, 0.42).
narrative_ontology:measurement(fr5hyper_tr_t30, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(fr5hyper_be_t0, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(fr5hyper_be_t15, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(fr5hyper_be_t30, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(fr5hyper_su_t0, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(fr5hyper_su_t15, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 15, 0.61).
narrative_ontology:measurement(fr5hyper_su_t30, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 30, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fifth_republic_constitution__hyper_presidential_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fifth_republic_constitution__hyper_presidential_reading, fifth_republic_constitution__parliamentary_constraint_reading).
narrative_ontology:affects_constraint(fifth_republic_constitution__hyper_presidential_reading, fifth_republic_constitution__cohabitation_equilibrium_reading).
narrative_ontology:affects_constraint(fifth_republic_constitution__hyper_presidential_reading, article_49_3_confidence_vote).
narrative_ontology:affects_constraint(fifth_republic_constitution__hyper_presidential_reading, article_16_emergency_decree_authority).

% DUAL FORMULATION NOTE:
% The Fifth Republic Constitution is a contested kernel with multiple structurally distinct readings. The hyper-presidential reading (this story) has ε=0.68 and classifies as Snare from most perspectives. The parliamentary_constraint_reading has lower ε (approximately 0.35-0.40) and classifies as Tangled Rope or Rope, emphasizing legislative authority and constitutional constraint. The cohabitation_equilibrium_reading has moderate ε (approximately 0.45-0.55) and reflects the empirical periods when divided government rebalances executive-legislative power. These are not alternative observables of a single constraint — they are separate constraint stories arising from fundamentally different constitutional readings. The network links them as a constraint family where the hyper-presidential reading structurally influences (and arguably forecloses over long time horizons) the parliamentary constraint reading through institutional drift, jurisprudential precedent, and electoral realignment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fifth_republic_constitution__hyper_presidential_reading, institutional, 0.1).
constraint_indexing:directionality_override(fifth_republic_constitution__hyper_presidential_reading, moderate, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
