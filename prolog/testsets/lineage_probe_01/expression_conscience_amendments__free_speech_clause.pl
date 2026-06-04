% ============================================================================
% CONSTRAINT STORY: expression_conscience_amendments__free_speech_clause
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_free_speech_clause, []).

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
 *   constraint_id: expression_conscience_amendments__free_speech_clause
 *   human_readable: Free Speech Clause: Open Marketplace of Expression
 *   domain: political/legal/constitutional
 *
 * SUMMARY:
 *   The Free Speech Clause of the First Amendment commits the polity to an
 *   open marketplace of expression in which government may not pick winners
 *   among viewpoints. This reading instantiates one of five structurally
 *   distinct commitments within the expression_conscience_amendments kernel
 *   (the others being assembly/petition rights, establishment, free exercise,
 *   and free press). The Free Speech Clause reading privileges speaker
 *   liberty and viewpoint-neutral government over protection of vulnerable
 *   populations from harmful speech. The constraint exhibits the core tension
 *   between maximizing protection for dissident and despised speech (rope
 *   benefit to speakers) and minimizing suppression of hate speech and
 *   dangerous expression (snare cost to targeted groups). The doctrine
 *   forbids government from suppressing speech on content or viewpoint
 *   grounds, even when that speech endangers or dehumanizes vulnerable
 *   populations. The marketplace framing assumes that truth and
 *   harm-correction emerge from unrestricted debate, and that government
 *   suppression of speech causes greater damage (enabling tyranny, preventing
 *   self-correction) than tolerating harmful speech. The empirical validity
 *   of the marketplace assumption is contested (omega variable:
 *   marketplace_epistemic_capacity). The constraint's extractiveness has
 *   risen from 0.32 to 0.52 over the 80-year interval (roughly 1944–2024),
 *   reflecting accumulating harms from hate speech, misinformation campaigns,
 *   and radicalization enabled by digital platforms. The suppression
 *   requirement (enforcement cost) has also risen as the state and private
 *   platforms face pressure to suppress harmful speech despite doctrinal
 *   prohibition. The theater ratio has remained moderate (0.38→0.48),
 *   indicating that First Amendment litigation is partly genuine
 *   constitutional reasoning and partly performative protection of doctrinal
 *   boundaries against mounting public pressure to regulate harmful speech.
 *
 * KEY AGENTS:
 *   - Speakers with despised/dissident viewpoints (institutional/arbitrage): Primary beneficiaries of the clause; experience it as pure coordination protecting them from government censorship
 *   - Targets of protected harmful speech (powerless/trapped): Primary victims; experience the clause as snare (extraction with suppression of exit); their suffering is constitutionally protected as the cost of free speech
 *   - Marginalized/vulnerable groups endangered by speech (powerless/trapped): Secondary victims; protected speech targeting them (vilification, dehumanization, conspiracy theories) endangers their safety and dignity
 *   - Mainstream institutional speakers (moderate/constrained): Mixed position; benefit from speech protection but experience extraction through association with hate speech and misinformation; constrained exit because denouncing free speech doctrine damages institutional credibility
 *   - Civil society counter-speech coalition (organized/constrained): Scaffold perspective; builds alternative verification mechanisms (fact-checking, counter-narrative campaigns, institutional guardrails) to enable speech correction without suppression
 *   - Government and enforcement apparatus (institutional/constrained): Hybrid position; forbidden from suppressing speech but forced to enforce boundaries between protected speech and punishable conduct (incitement, conspiracy); constrained by doctrinal prohibition
 *   - Judiciary and constitutional system (institutional/arbitrage): Maintains doctrinal stability through consistent application of content-neutral, viewpoint-neutral tests; high exit-like capacity through interpretation
 *   - Analytical observer (analytical/analytical): Risks naturalizing the free speech commitment as a law of political nature rather than a contestable political choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(expression_conscience_amendments__free_speech_clause, 0.52).
domain_priors:suppression_score(expression_conscience_amendments__free_speech_clause, 0.38).
domain_priors:theater_ratio(expression_conscience_amendments__free_speech_clause, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(expression_conscience_amendments__free_speech_clause, extractiveness, 0.52).
narrative_ontology:constraint_metric(expression_conscience_amendments__free_speech_clause, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(expression_conscience_amendments__free_speech_clause, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(expression_conscience_amendments__free_speech_clause, tangled_rope).
narrative_ontology:human_readable(expression_conscience_amendments__free_speech_clause, "Free Speech Clause: Open Marketplace of Expression").
narrative_ontology:topic_domain(expression_conscience_amendments__free_speech_clause, "political/legal/constitutional").

domain_priors:requires_active_enforcement(expression_conscience_amendments__free_speech_clause).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(expression_conscience_amendments__free_speech_clause, 'c97e76a1-2126-4cea-9d58-2d1b5a05c023').
narrative_ontology:cs_kernel_codification('c97e76a1-2126-4cea-9d58-2d1b5a05c023', fixed_text).
narrative_ontology:cs_authority_grounding('c97e76a1-2126-4cea-9d58-2d1b5a05c023', lineage).
narrative_ontology:cs_interpretation_layer_present('c97e76a1-2126-4cea-9d58-2d1b5a05c023').
narrative_ontology:cs_reading_relation('c97e76a1-2126-4cea-9d58-2d1b5a05c023', expression_conscience_amendments__assembly_petition_clause, influences).
narrative_ontology:cs_reading_relation('c97e76a1-2126-4cea-9d58-2d1b5a05c023', expression_conscience_amendments__establishment_clause, coexists_with).
narrative_ontology:cs_reading_relation('c97e76a1-2126-4cea-9d58-2d1b5a05c023', expression_conscience_amendments__free_exercise_clause, influences).
narrative_ontology:cs_reading_relation('c97e76a1-2126-4cea-9d58-2d1b5a05c023', expression_conscience_amendments__free_press_clause, influences).
narrative_ontology:cs_axiom('c97e76a1-2126-4cea-9d58-2d1b5a05c023', foundational, government_viewpoint_neutrality_mandatory).
narrative_ontology:cs_axiom_status(government_viewpoint_neutrality_mandatory, holdable).
narrative_ontology:cs_axiom_grounding('c97e76a1-2126-4cea-9d58-2d1b5a05c023', government_viewpoint_neutrality_mandatory, deontological).
narrative_ontology:cs_axiom('c97e76a1-2126-4cea-9d58-2d1b5a05c023', secondary, marketplace_self_correction_assumption).
narrative_ontology:cs_axiom_status(marketplace_self_correction_assumption, holdable).
narrative_ontology:cs_axiom_grounding('c97e76a1-2126-4cea-9d58-2d1b5a05c023', marketplace_self_correction_assumption, empirically_contingent).
narrative_ontology:cs_reference_frame('c97e76a1-2126-4cea-9d58-2d1b5a05c023', neutral_viewpoint_marketplace_framework).
narrative_ontology:cs_drift_state('c97e76a1-2126-4cea-9d58-2d1b5a05c023', digital_amplification_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c97e76a1-2126-4cea-9d58-2d1b5a05c023', '').
narrative_ontology:cs_kernel_id(expression_conscience_amendments__free_speech_clause, expression_conscience_amendments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(expression_conscience_amendments__free_speech_clause, speakers_especially_dissidents).
narrative_ontology:constraint_beneficiary(expression_conscience_amendments__free_speech_clause, despised_viewpoint_holders).
narrative_ontology:constraint_victim(expression_conscience_amendments__free_speech_clause, targets_of_protected_harmful_speech).
narrative_ontology:constraint_victim(expression_conscience_amendments__free_speech_clause, marginalized_groups_endangered_by_speech).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TARGETED VULNERABLE GROUP (SNARE) — Groups targeted by protected speech (e.g., religious minorities subject to vilification, marginalized populations exposed to dehumanization campaigns) experience the Free Speech Clause as pure extraction with suppression of exit. The doctrine forbids government from suppressing the harmful speech even when it endangers the target. Exit is structural (cannot leave the nation, cannot shed the targeted identity). The constraint extracts from this agent (their silence is enforced, their dignity undefended) to benefit speakers with despised views.
constraint_indexing:constraint_classification(expression_conscience_amendments__free_speech_clause, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DISSIDENT SPEAKER / DESPISED VIEWPOINT (ROPE) — The speaker holding a marginalized, unpopular, or dangerous viewpoint experiences the constraint as pure coordination: the clause guarantees that government will not use its monopoly on violence to silence them. This is a genuine coordination benefit — the alternative (government picking winners) requires constant surveillance and suppression. The beneficiary's exit options are strong (speech can be published, distributed, moved to alternative platforms); the extraction runs toward them. They experience the constraint as a fair coordination mechanism.
constraint_indexing:constraint_classification(expression_conscience_amendments__free_speech_clause, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: MAINSTREAM INSTITUTIONAL SPEAKER (TANGLED ROPE) — Established institutional speakers (media, academia, government officials) experience the clause as mixed coordination and extraction. The coordination function is real: protection from government censorship enables institutional journalism and scholarly communication. The extraction is embedded: the same doctrine that protects institutional speech also protects weaponized misinformation and hate speech that damages the institutional speaker's credibility and social trust. The mainstream speaker has constrained exit (high reputational cost to denounce free speech doctrine; cannot exit the commons without losing platform legitimacy).
constraint_indexing:constraint_classification(expression_conscience_amendments__free_speech_clause, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CIVIL SOCIETY COUNTER-SPEECH COALITION (SCAFFOLD) — Organized actors (civic organizations, fact-checkers, community defense networks, social media literacy campaigns) see the Free Speech Clause as a temporary constraint with a generational sunset: as counter-speech infrastructure matures, the need to suppress speech falls away because social-level correction mechanisms become robust enough that harmful speech loses power. The coalition experiences constrained exit (operating within the free speech framework is mandatory; cannot suppress speech without losing legitimacy) but sees a clear pathway: build superior counter-speech infrastructure, not censorship. Theater is relatively low (counter-speech is functional, not performative).
constraint_indexing:constraint_classification(expression_conscience_amendments__free_speech_clause, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: GOVERNMENT AND ENFORCEMENT APPARATUS (TANGLED ROPE) — The state apparatus experiences the constraint as hybrid. The coordination function is stabilization: the doctrine prevents government from becoming a party to viewpoint contestation, which would trigger arms races over state control. The extraction is embedded: by forbidding government from suppressing harmful speech, the clause ensures that certain dangerous expressions (incitement to violence, conspiracy, sedition) go partially unenforced because distinguishing them from protected speech is costly. Government is constrained (cannot exit the constitutional duty); both beneficiaries and victims are created by enforcement choices.
constraint_indexing:constraint_classification(expression_conscience_amendments__free_speech_clause, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: JUDICIAL-CONSTITUTIONAL SYSTEM (ROPE) — The judiciary and constitutional apparatus experience the clause as pure coordination at the civilizational time scale. The doctrine provides stable rules for adjudication (content-neutral, viewpoint-neutral tests). The judges have exit-like options through interpretation (narrow or broad readings, strict or permissive exceptions). From this perspective, the extraction is minimal because the coordinate function is to enable the constitutional system itself to function without collapsing into factional warfare over whose speech gets protected. Theater is moderate (doctrinal elaboration is partly genuine legal reasoning, partly performative refinement of already-settled principles).
constraint_indexing:constraint_classification(expression_conscience_amendments__free_speech_clause, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — An absolutist analytical view holds that the Free Speech Clause instantiates a natural law: that suppressing speech to prevent harm always causes greater harm (because it requires government control of information, which enables tyranny). From this view, the clause is immutable, unchosen, a logical law of political physics. But the structural data contradicts this: identifiable beneficiaries (speakers, especially despised speakers) and identifiable victims (targets of protected harmful speech) exist. The mountain classification is a false summit — it naturalizes a political choice (accepting some harms from harmful speech to prevent worse harms from censorship) as an unchangeable law.
constraint_indexing:constraint_classification(expression_conscience_amendments__free_speech_clause, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(expression_conscience_amendments__free_speech_clause_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(expression_conscience_amendments__free_speech_clause, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(expression_conscience_amendments__free_speech_clause, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(expression_conscience_amendments__free_speech_clause, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(expression_conscience_amendments__free_speech_clause_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high and rising. The Free Speech Clause creates a structural asymmetry: speakers with despised views are protected (beneficiaries), while targets of hate speech are exposed to harm without government defense (victims). The extractiveness of 0.52 reflects that the clause actively prevents government from protecting targeted groups, making it an extractive mechanism relative to victim experience. The rising trajectory (0.32→0.52) reflects that digital platforms have amplified the scale of harmful speech, making the same doctrine increasingly extractive as platforms enable rapid-scale vilification and radicalization. Suppression (0.38): Moderate. The clause does suppress government power (forbids content/viewpoint-based censorship), but does not suppress all constraint mechanisms — private deplatforming, social ostracism, counter-speech, and market-based speech correction remain available. The suppression is not total (0.38, not >0.60) because alternatives to government suppression are viable. The rising trajectory reflects that as pressure mounts to regulate harmful speech, enforcement of the speech-protection doctrine requires more active suppression of regulatory impulses. Theater ratio (0.48): Moderate. First Amendment doctrine is partly genuine constitutional reasoning (the marketplace rationale has a legitimate epistemic foundation; the harm principle marks a real boundary in political theory) and partly performative (the doctrine's application in digital-age contexts often treats platforms as neutral carriers despite their algorithmic curation; the marketplace framing ignores empirical failures of truth-correction). The rising trajectory reflects that as the doctrine's empirical assumptions fail (misinformation persists and spreads), maintaining the doctrine requires increasing performative reaffirmation.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is maximal. The dissident speaker experiences rope (pure coordination protecting them from state violence). The targeted vulnerable group experiences snare (extraction with suppression of exit). The mainstream institutional speaker experiences tangled rope (mixed benefit and extraction). The counter-speech coalition experiences scaffold (a temporary constraint with a generational sunset path). The government experiences tangled rope (coordination with embedded extraction). The judiciary experiences rope (stable doctrinal rules). The analytical absolutist risks mountain (naturalizing a political choice as a law of nature). No single classification captures the constraint — the presheaf over all perspectives reveals the structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are determined by beneficiary/victim status and exit options. Speakers with despised views are beneficiaries with strong exit (arbitrage) → low d → negative/minimal chi (they experience the clause as enabling, not extractive). Targeted groups are victims with no exit (trapped) → high d (0.95) → high f(d) (1.42) → high chi (they experience maximum extraction). Mainstream speakers are both beneficiaries (from speech protection) and partial victims (from hate speech's spread) with constrained exit → moderate d (0.50-0.55) → moderate chi. The government is neither pure beneficiary nor victim but constrained actor (forced to enforce boundaries) → derived d from constrained exit and mixed extraction/protection balance. The judicial system has arbitrage-like exit through interpretation → low d. Each perspective's chi is computed as ε × f(d) × σ(S), where S is national (σ=1.0 in the base calculation; international diffusion could raise σ).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the Free Speech Clause contains BOTH a genuine coordination function (protecting dissidents from state violence) AND asymmetric extraction (leaving vulnerable groups exposed to harm). The doctrine does not collapse into pure extraction (snare) nor pure coordination (rope) because BOTH functions coexist. From the dissident speaker's position, the clause coordinates at low cost. From the vulnerable target's position, the clause extracts at high cost. The mandatrophy arises from the attempt to view the clause as a single type; the resolution is to recognize that it is genuinely hybrid — a tangled rope that benefits some and harms others, with the benefits flowing to speakers and the harms flowing to speech targets. The doctrine explicitly embraces this asymmetry (First Amendment theory accepts that some speech causes harm but forbids using government power to suppress it). The constraint does not resolve the mandatrophy; it instantiates the political choice to accept victim extraction in service of speaker protection.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    marketplace_epistemic_capacity,
    'Does the open marketplace of expression actually correct falsehoods and harmful ideologies better than speech regulation, or does it amplify them?',
    'Empirical comparison: information ecosystems with high free speech protection vs. those with active misinformation suppression; measurement of false-belief persistence, radicalization rates, and collective epistemic outcomes across regimes',
    'If marketplace self-corrects: Free Speech Clause maximizes epistemic reliability (rope from more perspectives, lower extractiveness). If marketplace amplifies falsehood: the clause sacrifices epistemic reliability to other values (extractiveness rises for vulnerable groups, tangled_rope/snare classifications solidify).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marketplace_epistemic_capacity, empirical, 'Whether free speech marketplace self-corrects or amplifies falsehood').

omega_variable(
    harm_thresholds_for_suppression,
    'What constitutes sufficiently direct and imminent harm to justify suppression (incitement, conspiracy, immediate violence)? Where is the boundary drawn?',
    'Case law analysis: how courts distinguish protected speech from suppressible conduct; comparison of harm standards across constitutional democracies; tracking of cases where harm threshold was applied inconsistently or controversially',
    'If harm threshold is set high (imminent/direct only): current doctrine holds; beneficiaries and victims remain as classified. If threshold is set lower (incremental/cumulative harms): more speech becomes suppressible; victim experience shifts from snare to constrained; doctrine moves toward speech regulation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(harm_thresholds_for_suppression, conceptual, 'Boundary between protected speech and suppressible conduct').

omega_variable(
    collective_action_alternative_to_suppression,
    'Can counter-speech, deplatforming (private rather than government action), and social accountability mechanisms replace government suppression as harm-mitigation tools without sacrificing the open marketplace principle?',
    'Real-world testing: effectiveness of counter-speech campaigns, private deplatforming, and institutional guardrails at reducing harmful speech''s reach; measurement of whether social correction reaches scale before harms accumulate',
    'If alternative mechanisms work: the scaffold perspective (counter-speech sunset) becomes the dominant trajectory; extractiveness of the Free Speech Clause decreases as substitutes mature. If alternatives fail: the snare classification for vulnerable groups hardens; pressure for government intervention increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_action_alternative_to_suppression, empirical, 'Viability of counter-speech and social accountability as suppression alternatives').

omega_variable(
    zero_sum_vs_positive_sum_speech_dynamics,
    'Is the relationship between speaker freedom and audience protection zero-sum (more protection for speakers = less protection for vulnerable targets) or can institutional design create positive-sum outcomes?',
    'Institutional design analysis: comparison of speech regimes across democracies; identification of governance structures that protect both dissident speech AND vulnerable populations; measurement of polarization, trust, and democratic stability outcomes',
    'If zero-sum: current tangled_rope/snare classifications are inevitable; tradeoff is structural. If positive-sum: institutional design (robust counter-speech, privacy protection for vulnerable, literacy infrastructure) can reduce victim extraction while maintaining speaker protection; multiple perspectives shift toward rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(zero_sum_vs_positive_sum_speech_dynamics, conceptual, 'Whether speaker freedom and vulnerable protection are zero-sum or positive-sum').

omega_variable(
    reading_contest_kernel_framing,
    'This constraint is ONE reading of the expression_conscience_amendments kernel. Five sibling readings compete: assembly_petition_clause (collective action rights), establishment_clause (secular state), free_exercise_clause (religious practice), free_press_clause (institutional journalism), and free_speech_clause (open marketplace). Do these readings foreclose each other, coexist, or influence one another?',
    'Constitutional law case analysis: identification of doctrinal conflicts (e.g., free exercise vs. establishment in religious speech cases); mapping of how readings reinforce or undermine each other in practice; observation of which reading dominates in contestation vs. which recedes',
    'If readings foreclose (zero-sum competition): recognizing the sibling relationships reveals trade-offs in the constitutional text itself. If readings coexist: the kernel is genuinely pluralistic, and different readings apply in different domains. If readings influence (structured dependencies): some readings are upstream of others; the free speech clause''s role depends on which siblings are prioritized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_kernel_framing, conceptual, 'Structural relationships among the five readings of the expression_conscience_amendments kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(expression_conscience_amendments__free_speech_clause, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fsc_tr_t0, expression_conscience_amendments__free_speech_clause, theater_ratio, 0, 0.38).
narrative_ontology:measurement(fsc_tr_t40, expression_conscience_amendments__free_speech_clause, theater_ratio, 40, 0.43).
narrative_ontology:measurement(fsc_tr_t80, expression_conscience_amendments__free_speech_clause, theater_ratio, 80, 0.48).

% Extraction over time
narrative_ontology:measurement(fsc_be_t0, expression_conscience_amendments__free_speech_clause, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(fsc_be_t40, expression_conscience_amendments__free_speech_clause, base_extractiveness, 40, 0.42).
narrative_ontology:measurement(fsc_be_t80, expression_conscience_amendments__free_speech_clause, base_extractiveness, 80, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(fsc_su_t0, expression_conscience_amendments__free_speech_clause, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(fsc_su_t40, expression_conscience_amendments__free_speech_clause, suppression_requirement, 40, 0.32).
narrative_ontology:measurement(fsc_su_t80, expression_conscience_amendments__free_speech_clause, suppression_requirement, 80, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(expression_conscience_amendments__free_speech_clause, enforcement_mechanism).
narrative_ontology:affects_constraint(expression_conscience_amendments__free_speech_clause, assembly_petition_clause).
narrative_ontology:affects_constraint(expression_conscience_amendments__free_speech_clause, establishment_clause).
narrative_ontology:affects_constraint(expression_conscience_amendments__free_speech_clause, free_exercise_clause).
narrative_ontology:affects_constraint(expression_conscience_amendments__free_speech_clause, free_press_clause).

% DUAL FORMULATION NOTE:
% This constraint is one of five readings of the expression_conscience_amendments kernel. Each sibling reading (assembly, establishment, exercise, press) is a separate constraint story with its own epsilon values and beneficiary/victim structures. The free speech reading dominates contemporary constitutional practice, which constrains how the other readings can be applied. All five stories must be linked via network.affects_constraints to represent the kernel contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(expression_conscience_amendments__free_speech_clause, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
