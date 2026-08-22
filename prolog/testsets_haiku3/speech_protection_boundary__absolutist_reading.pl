% ============================================================================
% CONSTRAINT STORY: speech_protection_boundary__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_boundary__absolutist_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: speech_protection_boundary__absolutist_reading
 *   human_readable: Absolutist Speech Protection Boundary (Brandenburg Standard)
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   The absolutist reading instantiates the Brandenburg standard (Brandenburg
 *   v. Ohio, 1969): speech receives near-absolute protection unless it
 *   directly incites imminent lawless action. Protected speech includes hate
 *   speech, conspiracy rhetoric, even violent imagery — so long as it does
 *   not cross the imminent-action threshold. The constraint is CLAIMED as
 *   rope (genuine coordination around a bright-line rule that protects
 *   dissent and press) while the authored metrics describe substantially
 *   extractive operation (high base extractiveness, significant burden on
 *   minoritized communities and harassment targets). The divergence is
 *   structural: from the dissent-protection seat, the arrangement is genuine
 *   coordination; from the equity seat, the same rule operates as enforced
 *   externalization of harm. This is one reading of a contested kernel;
 *   sibling readings (harm_limited_reading, balancing_reading) would weight
 *   the harm externality differently and permit narrower exceptions.
 *
 * KEY AGENTS:
 *   - speech_producers_across_spectrum: All speakers benefit from the broad protection; political speech is maximally shielded (power: powerful, exit: mobile)
 *   - minoritized_communities: Racial, ethnic, and religious minorities experience systematic dehumanization speech as a protected-speech category; they bear harm as externality (power: moderate, exit: constrained)
 *   - harassment_targets: Individuals subjected to sustained targeted abuse fall outside legal remedy unless it meets imminent-action threshold; identity-locked exit (power: powerless, exit: identity_locked)
 *   - judiciary: Federal courts enforce the Brandenburg boundary; judicial interpretation determines what counts as imminent and lawless (power: institutional, exit: analytical)
 *   - legislative_branch: Excluded from narrowing the boundary through statute; any attempt faces strict scrutiny (power: institutional, exit: trapped)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_boundary__absolutist_reading, 0.68).
domain_priors:suppression_score(speech_protection_boundary__absolutist_reading, 0.22).
domain_priors:theater_ratio(speech_protection_boundary__absolutist_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_boundary__absolutist_reading, rope).
narrative_ontology:human_readable(speech_protection_boundary__absolutist_reading, "Absolutist Speech Protection Boundary (Brandenburg Standard)").
narrative_ontology:topic_domain(speech_protection_boundary__absolutist_reading, "constitutional/political").

domain_priors:requires_active_enforcement(speech_protection_boundary__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_boundary__absolutist_reading, '890f4b2f-0ac4-45c7-8df2-98df7cdf29d8').
narrative_ontology:cs_kernel_codification('890f4b2f-0ac4-45c7-8df2-98df7cdf29d8', fixed_text).
narrative_ontology:cs_authority_grounding('890f4b2f-0ac4-45c7-8df2-98df7cdf29d8', lineage).
narrative_ontology:cs_interpretation_layer_present('890f4b2f-0ac4-45c7-8df2-98df7cdf29d8').
narrative_ontology:cs_reading_relation('890f4b2f-0ac4-45c7-8df2-98df7cdf29d8', speech_protection_boundary__harm_limited_reading, coexists_with).
narrative_ontology:cs_reading_relation('890f4b2f-0ac4-45c7-8df2-98df7cdf29d8', speech_protection_boundary__balancing_reading, influences).
narrative_ontology:cs_axiom('890f4b2f-0ac4-45c7-8df2-98df7cdf29d8', foundational, imminent_lawless_action_sufficiency).
narrative_ontology:cs_axiom_status(imminent_lawless_action_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('890f4b2f-0ac4-45c7-8df2-98df7cdf29d8', imminent_lawless_action_sufficiency, deontological).
narrative_ontology:cs_axiom('890f4b2f-0ac4-45c7-8df2-98df7cdf29d8', foundational, speech_protection_categorical_maximization).
narrative_ontology:cs_axiom_status(speech_protection_categorical_maximization, holdable).
narrative_ontology:cs_axiom_grounding('890f4b2f-0ac4-45c7-8df2-98df7cdf29d8', speech_protection_categorical_maximization, deontological).
narrative_ontology:cs_reference_frame('890f4b2f-0ac4-45c7-8df2-98df7cdf29d8', brandenburg_bright_line_bright_line_imminent_action_floor).
narrative_ontology:cs_drift_state('890f4b2f-0ac4-45c7-8df2-98df7cdf29d8', contemporary_internet_scale, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('890f4b2f-0ac4-45c7-8df2-98df7cdf29d8', '').
narrative_ontology:cs_kernel_id(speech_protection_boundary__absolutist_reading, speech_protection_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_boundary__absolutist_reading, speech_producers_across_spectrum).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__absolutist_reading, press_institutions).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__absolutist_reading, political_dissident_movements).
narrative_ontology:constraint_victim(speech_protection_boundary__absolutist_reading, minoritized_communities).
narrative_ontology:constraint_victim(speech_protection_boundary__absolutist_reading, harassment_targets).
narrative_ontology:constraint_victim(speech_protection_boundary__absolutist_reading, equality_seekers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__absolutist_reading, counter_speech_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% All speakers — journalists, commentators, activists, ordinary citizens — operate under maximally permissive speech rules. The Brandenburg standard protects political speech, satire, hate speech, conspiracy speech, and incitement-adjacent speech so long as it stops short of direct calls for imminent lawless action. Benefit: ability to speak categorically, develop positions without constant fear of legal suppression, test ideas publicly. The speech producer's burden is low.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, speech_producers_across_spectrum, beneficiary,
    powerful, generational, mobile, national).

% Major newspapers, wire services, and broadcasters operate under the absolutist reading's broad shield. They can publish investigation, criticism, even inflammatory takes on political figures and movements without fear of prior restraint or libel liability (under actual-malice standard layered on top). Institutional autonomy and market reach are protected from government suppression.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, press_institutions, beneficiary,
    powerful, generational, mobile, national).

% Revolutionary groups, civil rights movements, anarchist collectives, and radical political voices benefit directly from the absolutist standard. Speech that would be suppressed under a balancing or harm-based standard — advocacy of systemic change, harsh critique of government, even inflammatory rhetoric about violence as political possibility — is protected. Their ability to organize, recruit, and propagandize is shielded.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, political_dissident_movements, beneficiary,
    moderate, biographical, mobile, national).

% Racial, ethnic, and religious minorities experience the absolutist standard as a constraint that permits organized, systematic dehumanization speech targeting their groups: slurs, conspiracy narratives, calls for separatism or removal (stopping short of imminent violence). They bear the aggregate harm — psychological, social, epistemic — as an externality of the maximally protected speech regime. Exit: they cannot leave the nation or opt out of the speech environment; they can only counter-speak or organize internally.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, minoritized_communities, payer,
    moderate, biographical, constrained, national).

% Individuals subjected to sustained, coordinated online and offline harassment campaigns — often based on identity (gender, sexuality, race, religion) — operate under a rule that protects the harasser's speech unless they meet the Brandenburg threshold (imminent, lawless action). Victims experience identity-locked exit: they cannot shed the identity that makes them targets. They can flee platforms or spaces but carry the targeted identity with them. The absolutist standard treats their harassment as speech rather than actionable harm.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, harassment_targets, payer,
    powerless, biographical, identity_locked, national).

% Movements working to advance equal protection under law (civil rights organizations, women's groups, LGBTQ+ organizations) argue that systematic exclusionary speech — speech that denies equal membership or iteratively marginalizes groups — undermines the equal-protection promise the speech doctrine itself is supposed to serve. They bear the constraint as a structural asymmetry: their own speech advocating equality can be framed as incitement, while speech denying their humanity is protected as political speech. Their exit: reform through constitutional amendment or court reversal, both high-friction.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, equality_seekers, payer,
    organized, generational, constrained, national).

% Federal courts, especially the Supreme Court, enforce and interpret the Brandenburg standard. They determine what counts as imminent, lawless, directed incitement. Their power is structural: a judicial shift in how Brandenburg is applied — whether it admits intent-to-incite or requires knowledge, whether timing matters, whether mob dynamics count — directly alters the constraint's enforcement scope. They set the jurisprudential boundary.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Congress is structurally excluded from narrowing the Brandenburg boundary through statute; any attempted restriction faces strict scrutiny and near-certain invalidation. Legislatures that have tried to criminalize hate speech, harassment, or harmful rhetoric have seen those laws struck down. Their exclusion is constitutive: they cannot enforce a harm-based or balancing standard because the judiciary has claimed the speech boundary as a constitutional floor, not a policy choice.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, legislative_branch, excluded,
    institutional, generational, trapped, national).

% Civil libertarians and marketplace-of-ideas theorists argue that the absolutist standard forces minoritized communities and harassment targets to rely on counter-speech, deplatforming, and social pressure rather than law. They frame this as a feature: the remedy for harmful speech is more speech, not suppression. They benefit from the maximally protected regime in principle, though they often ally with payer groups in practice.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, counter_speech_advocates, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_boundary__absolutist_reading, counter_speech_advocates, observer).

% Most democracies outside the United States maintain hate-speech laws, harassment prohibitions, or dignity-protecting speech restrictions more permissive than Brandenburg would allow. International human-rights bodies sometimes criticize U.S. speech policy as insufficiently protective of dignity and equality. They observe the trade-off the absolutist standard instantiates: maximum freedom of expression at the cost of equality protections.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, international_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_boundary__absolutist_reading, speech_producers_across_spectrum).
narrative_ontology:fixing_cost_class(speech_protection_boundary__absolutist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Brandenburg standard solves a collective-action problem: how to protect political speech and dissent against government suppression while maintaining a stable rule-of-law boundary that does not require case-by-case judgment about whether speech is politically inconvenient. The solution is a bright-line rule (imminent lawless action) that shifts the boundary far toward protection and away from balancing.
% TRANSFER_FUNCTION: The constraint transfers the right to unpoliced political speech from government authority to individual speakers. It transfers the burden of harm — psychological, social, epistemic harm from dehumanizing speech — from the state (which would police it) to the minoritized communities and harassment targets who experience it as an externality. The aggregate harm is borne diffusely by those targeted by systematic speech, not concentrated in any beneficiary seat.
% ABSENT_VOICES: Minoritized communities historically had minimal voice in the doctrinal development (Brandenburg arose from cases involving civil-rights protesters and left-wing radicals, but the doctrine's application generalizes beyond their protection). International human-rights bodies and scholars in other democracies question the trade-off but have no seat in U.S. constitutional interpretation. Harassment targets and equality advocates have entered the conversation more recently, especially post-internet, but their claims still encounter the doctrine as settled background.
% DISAPPEARANCE_RATIONALE: If the absolutist Brandenburg standard disappeared and were replaced by a harm-based or balancing standard, hate-speech laws and harassment protections would become enforceable; dissident speech would face new legal risk; majorities could suppress minority viewpoints more easily; the judiciary would have to adjudicate political speech case-by-case rather than apply a bright rule. The entire political-speech ecology would reorganize around different risk calculations.
% FOUNDING_PROBLEM: The founding problem was government suppression of political dissent and unpopular speech. Early sedition laws, obscenity prosecutions, and anti-communist speech restrictions were used to silence opposition. Brandenburg emerged from cases where the government prosecuted speech for its political content (the Klan, civil-rights activists). The doctrine was built to prevent government from weaponizing speech law against its opponents.
% FOUNDING_PROBLEM_CORROBORATION: Civil libertarians and free-speech advocates continue to attest that government suppression remains a live threat, citing campaigns against political speech, organized harassment of journalists, and efforts to criminalize protest. Minoritized communities and equality advocates attest that the founding problem is substantially solved — government is not the primary threat to their speech — and that the doctrine now operates as a shield for private systematic harm. The Supreme Court's jurisprudence privileges the dissent-protection reading; legislative and international bodies question whether that framing remains appropriate post-internet. Scholarly work outside the beneficiary seats (Delgado & Stefancic, Stanley, Sunstein) documents the shift in threat landscape from state censorship to private coordinated harassment.
narrative_ontology:disappearance_verdict(speech_protection_boundary__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_boundary__absolutist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_boundary__absolutist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(speech_protection_boundary__absolutist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_boundary__absolutist_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_boundary__absolutist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_boundary__absolutist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_boundary__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end) because the constraint imposes substantial aggregate harm on minoritized communities and harassment targets, and that harm is structurally internalized as an externality rather than remedied or distributed. The harm is not metered to the beneficiaries (speech producers) but to a powerless class. Suppression is low (0.22) because the constraint itself does NOT require active suppression of counter-speech or alternative movements — it forbids government suppression while permitting private speech. Theater is low-moderate (0.18): the constraint includes genuine rule-of-law function (bright-line boundary prevents arbitrary prosecution), but as the interval progresses, the doctrine's application increasingly serves to protect systematic harassment while maintaining a legitimacy claim of protecting dissent. The measurement trajectory shows extractiveness rising over 1925–2026 as the doctrine's application has generalized beyond its dissent-protection origin to shield organized hate speech and coordinated harassment, while suppression requirement has fallen (the constraint requires less active enforcement because the rule is now institutionally stable and generates minimal organized resistance from its beneficiaries). Theater remains low because the coordination function is real, not performative.
 *
 * PERSPECTIVAL GAP:
 *   From the dissent-protection seat (beneficiary), the constraint appears as rope: a genuine coordination mechanism that protects minority speech and prevents tyranny of the majority in legislative power. From the minoritized-community seat (payer), the same constraint appears as tangled_rope or snare: a rule that coordinates protection for political speech while extracting harm from groups who lack political power. The perspectival gap arises from asymmetric power and exit: speech producers who benefit from the constraint (press, dissident movements) have institutional resources and geographic mobility; harassment targets and minoritized communities are both structurally targeted by speech AND lack institutional or legal remedies. The engine will compute these divergences from the structural data: beneficiary seats with powerful/mobile power atoms will see low d (near-beneficiary end), while payer seats with powerless/constrained/identity-locked power atoms will see high d (near-target end). The claim/metric independence principle is critical here: I am claiming rope (the dissent-protection function is genuine) while authoring metrics that describe high extractiveness and significant burden on powerless actors — the engine's per-seat computation will surface the conflict.
 *
 * DIRECTIONALITY LOGIC:
 *   Speech producers and press institutions are structural beneficiaries (d near 0.0): they benefit from protection without running the constraint, and their exit is mobile (they can relocate, change topics, adopt safer speech). Political dissidents are beneficiaries in the same structural sense, though their power is moderate: they collect protection against government suppression, but their exit is constrained by the political commitment itself. Minoritized communities and harassment targets are structural payers (d near 1.0): they bear the harm of protected dehumanization speech, their exit is constrained (cannot leave the nation) or identity-locked (cannot shed the targeted identity), and the harm is diffuse and externalized. The judiciary sits as agenda-setter (d symmetric, ~0.5): they administer the boundary, their power is institutional, their exit is analytical (they can change interpretation but cannot opt out of the role). The legislative branch is excluded (trapped, cannot exit the constitutional system), so they experience the constraint as a structural lock on their power.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was government suppression of political speech. Brandenburg was built to solve that problem. The mandatrophy question is: has the problem been solved, and does the doctrine still serve its original function, or has it persisted past the point where it serves that function and now primarily vindicates a maximalist principle? The contested founding_problem_status reflects this: civil libertarians attest the threat of government suppression remains live (the founding problem persists); minoritized communities and equality advocates attest the founding problem is substantially solved and the doctrine now operates orthogonal to its original purpose (the problem is dead). The misalignment between founding_problem_status=contested and disappearance_verdict=world_rearranges suggests the doctrine is in a mandatrophy zone: its persistence depends on continued commitment to the maximalist principle from powerful beneficiaries (press, institutional speakers), not on the continued salience of the founding problem. If the founding problem were no longer live, and if minoritized communities and harassment targets could demonstrate that the doctrine's continuation is not necessary to prevent government suppression, the constraint would face pressure to evolve (toward a harm-based or balancing standard). The doctrine's inertia is institutional: the judiciary has invested in the Brandenburg framework, political dissident movements benefit from it, and major press institutions treat it as foundational to their role.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harm_externalization_necessity,
    'Is the harm externality to minoritized communities a necessary feature of the bright-line Brandenburg rule, or could a slightly narrower imminent-action standard protect dissent while mitigating systematic harassment?',
    'Empirical comparison with jurisdictions that maintain narrower speech exceptions (hate speech, harassment laws) alongside strong political-speech protections: do they show measurably weaker dissent protection or stronger equality outcomes? Natural experiment from constitutional amendments that narrowed Brandenburg.',
    'If the narrower exceptions are compatible with strong dissent protection, the absolute necessity of the harm externality is undermined, and the constraint becomes more clearly snare than rope. If narrower exceptions measurably weaken dissent protection, the trade-off is genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_externalization_necessity, empirical, 'Whether the constraint''s externalized harm is a necessary feature of protecting dissent or an avoidable side effect.').

omega_variable(
    foundational_versus_derivative_threat,
    'Is government suppression of political speech the foundational threat to democracy that Brandenburg addresses, or is democratic erosion now more directly threatened by systematic private harassment and epistemic exclusion that the absolutist rule enables?',
    'Historical-comparative analysis of democracies that collapsed: what role did government speech suppression play versus private violence and social fragmentation? Institutional analysis of how private-sector speech platforms shape political speech relative to government restriction.',
    'If private harm has become the more direct threat, the founding_problem (government suppression) would be reclassified as dead, and the constraint would be exposed as persisting on inertia rather than necessity. If government suppression remains primary, the founding problem remains live.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(foundational_versus_derivative_threat, conceptual, 'Whether the constraint''s founding justification remains the most salient threat to democracy.').

omega_variable(
    reading_stability_under_scale,
    'Does the Brandenburg standard remain coherent and equally protective of dissent when scaled from print and broadcast media to internet-era mass coordination and algorithmic amplification? Does imminent-action remain a meaningful boundary when speech reaches distributed audiences across months and years rather than through organized chains of command?',
    'Case-law analysis of how Brandenburg has been applied to internet harassment, coordinated incel/QAnon/white-nationalist speech, and algorithmic radicalization: does the standard capture new forms of incitement? Experimental evidence on whether audiences experience algorithmic-amplified dehumanization speech as a form of coordinated pressure.',
    'If imminent-action becomes meaningless under internet scale, the constraint''s coherence breaks down. If it remains coherent, the doctrine''s applicability extends to new speech modalities. Either way, the constraint''s meaning and extraction profile change with the technology environment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_stability_under_scale, conceptual, 'Whether the Brandenburg standard''s operative meaning persists across communication technologies.').

omega_variable(
    democratic_participation_asymmetry,
    'Does the absolutist reading''s protection of hate speech and dehumanization create a structural asymmetry in democratic participation: maximizing voice for powerful speakers while marginalizing the political voice of targets, who experience the speech environment as hostile and exclusionary?',
    'Empirical study of political participation rates among minoritized groups in jurisdictions with strong hate-speech laws versus absolutist regimes. Survey data on whether harassment targets exit political engagement when exposed to systematic dehumanization speech. Comparative study of democratic equality across legal regimes.',
    'If asymmetric exclusion is confirmed, the constraint operates to maximize formal speech freedom at the cost of effective political equality. If minoritized communities show equal participation despite hostile speech, the externality is real but the participation cost is not prohibitive. This bears on whether the trade-off is justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_participation_asymmetry, empirical, 'Whether protecting all speech creates structural inequality in democratic voice.').

omega_variable(
    reading_foreclosure_condition,
    'Would the harm_limited or balancing readings logically foreclose the absolutist reading within a single constitutional framework, or are they genuinely alternative commitments that different parties can hold simultaneously?',
    'Formal logical analysis of the three readings'' foundational axioms: does holding one axiom require rejecting another''s core premise? Constitutional history: have courts explicitly chosen among these readings, or have they tried to reconcile them?',
    'If foreclosure is true, only one reading can be constitutional law; the others are errors. If they coexist without logical contradiction, the choice among readings is political and value-indexed, not discovery-indexed. The classification of the relationship (forecloses vs. coexists_with) depends on this analysis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_condition, conceptual, 'Structural relationship between the absolutist reading and its siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_boundary__absolutist_reading, 1925, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t1925, speech_protection_boundary__absolutist_reading, theater_ratio, 1925, 0.05).
narrative_ontology:measurement(spee_tr_t1954, speech_protection_boundary__absolutist_reading, theater_ratio, 1954, 0.08).
narrative_ontology:measurement(spee_tr_t1978, speech_protection_boundary__absolutist_reading, theater_ratio, 1978, 0.12).
narrative_ontology:measurement(spee_tr_t1995, speech_protection_boundary__absolutist_reading, theater_ratio, 1995, 0.14).
narrative_ontology:measurement(spee_tr_t2012, speech_protection_boundary__absolutist_reading, theater_ratio, 2012, 0.16).
narrative_ontology:measurement(spee_tr_t2026, speech_protection_boundary__absolutist_reading, theater_ratio, 2026, 0.18).

% Extraction over time
narrative_ontology:measurement(spee_be_t1925, speech_protection_boundary__absolutist_reading, base_extractiveness, 1925, 0.15).
narrative_ontology:measurement(spee_be_t1954, speech_protection_boundary__absolutist_reading, base_extractiveness, 1954, 0.25).
narrative_ontology:measurement(spee_be_t1978, speech_protection_boundary__absolutist_reading, base_extractiveness, 1978, 0.45).
narrative_ontology:measurement(spee_be_t1995, speech_protection_boundary__absolutist_reading, base_extractiveness, 1995, 0.55).
narrative_ontology:measurement(spee_be_t2012, speech_protection_boundary__absolutist_reading, base_extractiveness, 2012, 0.62).
narrative_ontology:measurement(spee_be_t2026, speech_protection_boundary__absolutist_reading, base_extractiveness, 2026, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t1925, speech_protection_boundary__absolutist_reading, suppression_requirement, 1925, 0.35).
narrative_ontology:measurement(spee_su_t1954, speech_protection_boundary__absolutist_reading, suppression_requirement, 1954, 0.3).
narrative_ontology:measurement(spee_su_t1978, speech_protection_boundary__absolutist_reading, suppression_requirement, 1978, 0.25).
narrative_ontology:measurement(spee_su_t1995, speech_protection_boundary__absolutist_reading, suppression_requirement, 1995, 0.23).
narrative_ontology:measurement(spee_su_t2012, speech_protection_boundary__absolutist_reading, suppression_requirement, 2012, 0.22).
narrative_ontology:measurement(spee_su_t2026, speech_protection_boundary__absolutist_reading, suppression_requirement, 2026, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_boundary__absolutist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(speech_protection_boundary__absolutist_reading, 0.08).
narrative_ontology:affects_constraint(speech_protection_boundary__absolutist_reading, speech_protection_boundary__harm_limited_reading).
narrative_ontology:affects_constraint(speech_protection_boundary__absolutist_reading, speech_protection_boundary__balancing_reading).
narrative_ontology:affects_constraint(speech_protection_boundary__absolutist_reading, government_speech_suppression_institutional_capacity).

% DUAL FORMULATION NOTE:
% The speech_protection_boundary kernel decomposes into three structurally distinct readings, each with different protected sets, different externality profiles, and different stakeholder relationships. The absolutist_reading maximizes protection and externality; harm_limited_reading shrinks protection to exclude certain dehumanization; balancing_reading case-by-case weights. Sibling relationships: absolutist coexists_with harm_limited (both held by different parties, neither forecloses the other within a single framework); absolutist influences balancing (the absolutist doctrine sets the baseline against which balancing is theorized as a deviation). Each reading has its own constraint_id, its own ε, its own beneficiary/victim structure. The readings are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_protection_boundary__absolutist_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
