% ============================================================================
% CONSTRAINT STORY: speech_harm_boundary__dignity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_harm_boundary__dignity_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: speech_harm_boundary__dignity_reading
 *   human_readable: Dignity-Reading Speech Boundary: Categorical Exclusion of Personhood-Denying Expression
 *   domain: constitutional_law/political_philosophy/communication_ethics
 *
 * SUMMARY:
 *   This file instantiates ONE reading of the speech_harm_boundary kernel:
 *   the dignity_reading, in which speech protection is subordinate to human
 *   dignity and personhood-denying expression (Holocaust denial, racist
 *   incitement, group defamation) sits categorically outside protection. The
 *   standing arrangement under contest, and the epsilon referent, is the
 *   actual dignity-based regime as institutionalized in German and wider
 *   European practice: a constitutional dignity guarantee ranked above
 *   expressive liberty, categorical criminal offenses for incitement and
 *   denial, platform takedown duties with short deadlines and fines, and
 *   supranational reinforcement through abuse-of-rights doctrines. Assessed
 *   by this reading's own lights, the regime confiscates expressive liberty
 *   in defined categories from a defined class of speakers and converts it
 *   into dignitary security for targeted groups; the reading endorses that
 *   trade, and the metrics below describe it without endorsement. The sibling
 *   readings (absolutist_reading, harm_balancing_reading) are separate
 *   constraints in separate files with their own epsilon, victim sets, and
 *   enforcement forms; nothing about them is averaged into this story.
 *   Claim/metric independence holds: the claimed type is what I believe
 *   structurally true of this arrangement (a genuine coordination function
 *   fused with asymmetric extraction under active enforcement), and the
 *   metrics are what I believe descriptively true of its operation.
 *
 * KEY AGENTS:
 *   - militant_democracy_state: agenda-setter (institutional/arbitrage) — writes, enforces, and finances the categorical exclusions; collects fines and legitimation
 *   - targeted_minority_groups: primary beneficiary (moderate/constrained) — the protected objects of the exclusion
 *   - holocaust_memory_communities: beneficiary (organized/constrained) — memory institutions shielded from denial
 *   - ordinary_citizens: beneficiary with diffuse payer side (moderate/constrained) — inhabit the protected environment, fund and chill under it
 *   - identity_harm_speakers: primary target (powerless/trapped) — bear categorical confiscation of expressive liberty with no lawful channel
 *   - boundary_case_dissenters: secondary target (organized/constrained) — political speakers under classification uncertainty; the visible resistance
 *   - platform_operators: compelled enforcer, mixed position (powerful/arbitrage) — bear compliance costs, gain certainty and incumbent advantage
 *   - absolutist_free_speech_advocates: excluded voice (organized/constrained) — hold the rival premise with no institutional seat inside this framework
 *   - comparative_constitutional_scholars: analytical observer (analytical/analytical) — measure the arrangement against its siblings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_harm_boundary__dignity_reading, 0.68).
domain_priors:suppression_score(speech_harm_boundary__dignity_reading, 0.75).
domain_priors:theater_ratio(speech_harm_boundary__dignity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_harm_boundary__dignity_reading, tangled_rope).
narrative_ontology:human_readable(speech_harm_boundary__dignity_reading, "Dignity-Reading Speech Boundary: Categorical Exclusion of Personhood-Denying Expression").
narrative_ontology:topic_domain(speech_harm_boundary__dignity_reading, "constitutional_law/political_philosophy/communication_ethics").

domain_priors:requires_active_enforcement(speech_harm_boundary__dignity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_harm_boundary__dignity_reading, '92d23d41-52a3-4fc5-b94a-7568b3f8a6a0').
narrative_ontology:cs_kernel_codification('92d23d41-52a3-4fc5-b94a-7568b3f8a6a0', fixed_text).
narrative_ontology:cs_authority_grounding('92d23d41-52a3-4fc5-b94a-7568b3f8a6a0', lineage).
narrative_ontology:cs_interpretation_layer_present('92d23d41-52a3-4fc5-b94a-7568b3f8a6a0').
narrative_ontology:cs_reading_relation('92d23d41-52a3-4fc5-b94a-7568b3f8a6a0', speech_harm_boundary__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('92d23d41-52a3-4fc5-b94a-7568b3f8a6a0', speech_harm_boundary__harm_balancing_reading, influences).
narrative_ontology:cs_axiom('92d23d41-52a3-4fc5-b94a-7568b3f8a6a0', foundational, human_dignity_trumps_expression).
narrative_ontology:cs_axiom_status(human_dignity_trumps_expression, holdable).
narrative_ontology:cs_axiom_grounding('92d23d41-52a3-4fc5-b94a-7568b3f8a6a0', human_dignity_trumps_expression, deontological).
narrative_ontology:cs_axiom('92d23d41-52a3-4fc5-b94a-7568b3f8a6a0', secondary, personhood_denial_outside_protection).
narrative_ontology:cs_axiom_status(personhood_denial_outside_protection, holdable).
narrative_ontology:cs_axiom_grounding('92d23d41-52a3-4fc5-b94a-7568b3f8a6a0', personhood_denial_outside_protection, deontological).
narrative_ontology:cs_reference_frame('92d23d41-52a3-4fc5-b94a-7568b3f8a6a0', militant_democracy_dignity_supremacy).
narrative_ontology:cs_drift_state('92d23d41-52a3-4fc5-b94a-7568b3f8a6a0', contemporary_platform_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('92d23d41-52a3-4fc5-b94a-7568b3f8a6a0', '').
narrative_ontology:cs_kernel_id(speech_harm_boundary__dignity_reading, speech_harm_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_harm_boundary__dignity_reading, targeted_minority_groups).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__dignity_reading, holocaust_memory_communities).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__dignity_reading, ordinary_citizens).
narrative_ontology:constraint_victim(speech_harm_boundary__dignity_reading, identity_harm_speakers).
narrative_ontology:constraint_victim(speech_harm_boundary__dignity_reading, boundary_case_dissenters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__dignity_reading, platform_operators).
narrative_ontology:constraint_victim(speech_harm_boundary__dignity_reading, ordinary_citizens).
narrative_ontology:constraint_victim(speech_harm_boundary__dignity_reading, platform_operators).
narrative_ontology:constraint_vindicates(speech_harm_boundary__dignity_reading, militant_democracy_doctrine).
narrative_ontology:constraint_vindicates(speech_harm_boundary__dignity_reading, human_dignity_supremacy_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Writes and enforces the categorical exclusions: criminalizes incitement to hatred and Holocaust denial, orders platforms to remove flagged content on short deadlines, prosecutes violators, and collects administrative fines. Grounds the entire arrangement in a constitutional dignity guarantee the constitution declares unamendable. Gains enforcement authority, fine revenue, and international standing as a state that protects dignity; bears the reputational and litigation costs of defending contested applications. Abandoning the arrangement is not a live option inside its own self-conception.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, militant_democracy_state, agenda_setter,
    institutional, generational, arbitrage, national).

% Live as the objects the exclusions shield: Jews, Muslims, Roma and Sinti, Black citizens, migrants, and other groups whose personhood the prohibited speech denies. Receive removal of denial and dehumanization from the public sphere they must inhabit. Cannot opt out of the discourse environment, and enforcement does not reliably reach coded variants or private channels, so protection is partial even where categorical on paper.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, targeted_minority_groups, beneficiary,
    moderate, generational, constrained, national).

% Survivor organizations, memorial sites, and Jewish community institutions. Benefit from criminal protection of historical memory against denial; supply testimony and expertise that enforcement relies on; and carry the recurring burden of re-engaging the history every time a denial case surfaces publicly.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, holocaust_memory_communities, beneficiary,
    organized, generational, constrained, national).

% Inhabit the protected discourse environment daily and broadly support it. Pay indirectly: taxes fund prosecution and platform oversight, and the chilling effect reaches past the excluded categories into adjacent political topics where speakers cannot predict how their speech will be classified. Emigration is not a realistic response to discourse norms.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, ordinary_citizens, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(speech_harm_boundary__dignity_reading, ordinary_citizens, payer).

% People who wish to assert the denied propositions: Holocaust denial, racial hierarchy, group dehumanization. Face criminal liability with no lawful channel inside the jurisdiction — no balancing hearing, no truth or intent defense that reliably avails — and relocating abroad does not restore domestic expression. Open organization around these propositions is itself the prohibited act, so the class cannot aggregate to defend itself.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, identity_harm_speakers, payer,
    powerless, biographical, trapped, national).

% Journalists, academics, and activists whose criticism targets policies of or toward protected groups — Middle East policy debate, boycott campaigns, sharp official-language critiques. Their speech is lawful until adjudicated otherwise, and adverse findings carry professional, platform, and reputational consequences. They litigate, publish, and contest classifications; they are the visible, organized resistance operating inside the system.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, boundary_case_dissenters, payer,
    organized, biographical, constrained, national).

% Operate the infrastructure where the regulated speech occurs. Must run around-the-clock takedown pipelines, translate statutory categories into content decisions at scale, and manage fine exposure for under-removal against backlash for over-removal. Gain legal certainty, incumbent advantages from compliance costs smaller rivals cannot carry, and insulation from coordinated boycott pressure. Can arbitrage: geofence obligations by jurisdiction, localize terms of service, and shift liability through intermediaries.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, platform_operators, payer,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(speech_harm_boundary__dignity_reading, platform_operators, beneficiary).

% Hold that expression is protected near-absolutely and that categorical exclusions are the founding error. Inside this jurisdiction their position holds no institutional seat: no major party adopts it, and courts treat it as closed by the dignity guarantee. They argue from outside — comparative commentary, international forums, and scholarship addressed to other jurisdictions' courts — which is where their objection actually circulates.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, absolutist_free_speech_advocates, excluded,
    organized, generational, constrained, continental).

% Study the arrangement against its siblings in other systems. Map where the categorical floor diverges from proportionality regimes, track boundary-case outcomes over time, and supply the external vantage from which the arrangement's drift is measurable. Neither collect from nor bear the arrangement; their stake is descriptive accuracy.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_harm_boundary__dignity_reading, militant_democracy_state).
narrative_ontology:fixing_cost_class(speech_harm_boundary__dignity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a shared floor of mutual recognizability in public discourse: a pluralistic society pre-commits, categorically and in advance, that attacks on the personhood of defined groups fall outside protected discourse, so that members of vulnerable groups can participate in public life without first litigating their own standing, and so that dehumanization cannot be used to organize persecution.
% TRANSFER_FUNCTION: Confiscates expressive liberty in the excluded categories from speakers, backed by criminal exposure and platform-enforced removal, and converts it into dignitary security for targeted groups; compliance costs move to platform operators and enforcement costs to the public treasury; fine revenue and legitimation accrue to the state.
% ABSENT_VOICES: Absolutist free-speech advocates are structurally outside the conversation: the categorical form of the constraint defines their premise as unspeakable within it rather than answering it, so they speak only from comparative and international venues. The speakers themselves hold no seat at all — the framework treats their utterance as the harm, so the person whose liberty is confiscated never appears as a claimant to be heard. Boundary-case dissenters sit half-in: heard only insofar as they can show their speech is not group-directed, which loads the uncertainty onto them.
% DISAPPEARANCE_RATIONALE: Overnight repeal would visibly rearrange the discourse environment: prohibited categories would re-enter circulation faster than informal norms could absorb them, targeted groups would experience an immediate security change, platforms would face contradictory obligations across jurisdictions, and the legislature would come under immediate pressure to re-legislate in some form — the arrangement's absence would be an event, not a nullity.
% FOUNDING_PROBLEM: Weimar: a political movement used legally protected speech to dehumanize Jews and other groups and then to organize their destruction, while the constitutional order lacked any tool to stop the speech stage of the process. The post-war framers concluded that a democracy must be able to defend the human dignity that precedes it, and built categorical dignity protection into the constitutional order as a permanent pre-commitment.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: national criminal-statistics offices record sustained antisemitic and racist incident levels; international monitoring bodies (ECRI, UN CERD reporting cycles) document persistent group-directed hostility in the jurisdiction; and independent historiography attests the Weimar mechanism the arrangement was built against. None of these sources depends on the arrangement's continuation for its standing, and none is a beneficiary of it.
narrative_ontology:disappearance_verdict(speech_harm_boundary__dignity_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_harm_boundary__dignity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_harm_boundary__dignity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_harm_boundary__dignity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_harm_boundary__dignity_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_harm_boundary__dignity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_harm_boundary__dignity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_harm_boundary__dignity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.68: the exclusion is categorical, not balanced — within the covered categories there is no hearing at which truth, intent, or context reliably restores protection, and criminal exposure attaches. That is heavy confiscation of expressive liberty from the payer class; it is moderated by the class's narrowness relative to all speakers, which is why this is not higher. Suppression 0.75: structural machinery (criminal law, platform deadlines, fine exposure) leaves no lawful channel inside the jurisdiction, and socialization adds an internalized chilling extension beyond the formal scope. Theater_ratio 0.28: enforcement is predominantly functional (prosecutions occur, takedowns occur at scale); a growing performative share comes from symbolic prosecutions and memorial-affirming proceedings whose deterrent yield is unclear. Accessibility_collapse 0.58: once the constraint is understood, lawful alternatives for the excluded categories collapse almost completely inside the jurisdiction, but coded language, foreign-hosted channels, and private settings persist, so alternatives degrade rather than vanish. Resistance 0.45: constitutional complaints, academic critique, and boundary-case litigation are continuous and organized, contained by broad public support rather than absent. Per the scaling rule, suppression is authored as a raw structural property and is NOT scaled by power or scope; only extractiveness is scaled by the engine through directionality and spatial scope. The measurement series run on one shared grid (points 0-50, roughly mapping the 1970s consolidation of denial and incitement offenses through the platform-enforcement era of the late 2010s and 2020s); all three tracked metrics are authored at every point. Trajectories are monotonic ratchets, not cycles: scope expansions and enforcement infrastructure accumulated stepwise, with no observed relaxation phase, so no cyclical-measurement apparatus applies.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently and the divergence is the finding. From the identity_harm_speaker seat the arrangement is categorical silencing: no balancing, no exit, liability without a hearing. From the targeted_minority_group seat the same structure is existential protection: the precondition of participating in public life at all. From the agenda_setter seat it is neither cost nor benefit but constitutive self-definition — the state cannot describe itself without this arrangement. The engine computes per-seat classifications from the structural data; this story authors the data and declines to adjudicate which seat's experience is 'the' constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. The state sits near the beneficiary end: it sets the rules, collects fines, and converts enforcement into legitimation. Targeted minority groups and memory communities are strong beneficiaries with constrained exit — they cannot leave the discourse environment they live in, which anchors them deep on the subsidized side. Ordinary citizens are declared beneficiaries but carry real diffuse costs (tax-funded enforcement, chilling spillover); the derivation will read them lower-cost than they are, a residual imprecision noted here rather than forced with an override, since the override axis (power atom) cannot separate them from other moderate-power beneficiaries. Identity_harm_speakers are full targets: trapped, because no lawful channel exists domestically and relocation does not restore domestic expression. Boundary_case_dissenters are high targets: organized enough to resist, constrained enough to lose. One override is declared: platform_operators derive a near-full-target directionality from their payer role, but their actual position is mixed — they monetize the moderation regime, gain legal certainty, enjoy compliance-cost advantages over smaller rivals, and can arbitrage across jurisdictions — so their d is corrected to 0.58, near symmetric and slightly target-side. The override exists because the derivation chain reads the payer declaration and cannot see the offsetting gains described in the stakeholder situation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Weimar: protected speech weaponized to dehumanize and then destroy) is live — corroborated externally by sustained hate-crime statistics and international monitoring — so the mandate has not outlived its function and no mandatrophy resolution is declared. The mismatch consumer reads founding_problem_status=live against disappearance_verdict=world_rearranges: no dead-mandate flag fires. The tangled_rope classification is what prevents both standard mislabels: calling this a pure rope would hide the categorical confiscation borne by the speaker class; calling it a pure snare would erase the genuine coordination function (a pre-committed dignity floor that vulnerable group members do not have to litigate individually) and the broad net-beneficiary population. The arrangement holds because both descriptions are true of the same structure, and the classification keeps both in view. Coalition note: the primary payer class is deliberately uncoalitionable — open organization around the excluded propositions is itself the prohibited act — so powerless payers here cannot aggregate; the coalition-relevant payers are the boundary_case_dissenters, whose press, academic, and litigating capacity is precisely the resistance the resistance metric registers. Identity-lock dynamics: the state's attachment is institutional identity fusion — militant democracy is constitutive of the post-war constitutional self-conception, so exit is unthinkable within the framework rather than merely costly; if that identity frame broke (a generational shift treating the dignity guarantee as historical artifact rather than living commitment), the arrangement would recompute as an ordinary balancing regime and the categorical structure would lose its entrenchment.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This story instantiates the dignity_reading of the speech_harm_boundary kernel; what structurally changes if a sibling reading (absolutist_reading, harm_balancing_reading) were adopted instead?',
    'Each sibling is authored as its own epsilon-invariant constraint file; comparison across the family resolves the delta. The disagreement is located in whether any categorical (non-balanced) exclusion exists and what grounds it: this reading says yes, grounded in deontological dignity supremacy; the harm_balancing_reading says no category is exempt from proportionality; the absolutist_reading says the override threshold is nearly unreachable.',
    'Under the harm_balancing_reading the victim set shrinks to demonstrated-harm cases and speaker burden becomes case-by-case; under the absolutist_reading categorical exclusions vanish and the measured extraction collapses toward the coordination floor. Epsilon, victim sets, and enforcement form all recompute per sibling file.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one reading of a three-reading kernel; sibling deltas documented here rather than folded into this constraint.').

omega_variable(
    categorical_scope_creep,
    'Does the categorical exclusion remain confined to personhood-denying speech, or does it systematically expand into adjacent political dissent (policy criticism of or toward protected groups, boycott advocacy, insult proceedings against officials)?',
    'Longitudinal coding of prosecutions, takedown decisions, and appellate outcomes for boundary cases: classify each contested application as personhood-denial-core or dissent-adjacent, and track the dissent-adjacent share over the interval.',
    'A rising dissent-adjacent share indicates the coordination story increasingly covers suppression of ordinary political conflict, drifting the constraint toward pure extraction with dignity language as cover; a stable low share supports the tangled_rope reading with a genuine coordination core.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_scope_creep, empirical, 'Whether the dignity floor functions as a principled limit or an expandable warrant.').

omega_variable(
    suppression_mechanism_split,
    'Is the measured suppression structural (criminal liability, platform removal duties, absence of any lawful channel) or internalized (socialization into dignity norms producing self-censorship beyond the formal scope)?',
    'Post-exit suppression trajectory: compare self-reported willingness to discuss contested topics in jurisdictions that decriminalized equivalent categories, and survey chilling effects among speakers whose subject matter is lawful but classification-adjacent. Persistence of avoidance after formal repeal indicates internalized carryover.',
    'If a substantial share is internalized, effective suppression exceeds the structural measure and would survive formal liberalization; the constraint''s footprint on discourse is larger than its statute book, and repeal remedies would under-deliver.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_split, empirical, 'Structural versus internalized components of the suppression scalar.').

omega_variable(
    dignity_ground_vs_order_ground,
    'Is the operative warrant for exclusion human dignity (deontological, confining the scope to attacks on personhood) or public order and social peace (consequentialist, admitting any disturbance rationale)?',
    'Doctrinal analysis of judicial reasoning: identify whether courts ever sustain an exclusion on order grounds alone, without finding an attack on personhood; track the ratio of dignity-grounded to order-grounded holdings.',
    'If order grounds independently suffice, the categorical structure loses its principled limit and any disturbance-justified restriction becomes admissible, widening effective scope and raising long-run extraction; if dignity grounds are necessary, the arrangement retains a constraining principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dignity_ground_vs_order_ground, conceptual, 'Which ground actually drives application, and therefore how wide the exclusion can grow.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_harm_boundary__dignity_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dignity_reading_tr_t0, speech_harm_boundary__dignity_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(dignity_reading_tr_t10, speech_harm_boundary__dignity_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement(dignity_reading_tr_t20, speech_harm_boundary__dignity_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(dignity_reading_tr_t30, speech_harm_boundary__dignity_reading, theater_ratio, 30, 0.23).
narrative_ontology:measurement(dignity_reading_tr_t40, speech_harm_boundary__dignity_reading, theater_ratio, 40, 0.26).
narrative_ontology:measurement(dignity_reading_tr_t50, speech_harm_boundary__dignity_reading, theater_ratio, 50, 0.28).

% Extraction over time
narrative_ontology:measurement(dignity_reading_be_t0, speech_harm_boundary__dignity_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(dignity_reading_be_t10, speech_harm_boundary__dignity_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(dignity_reading_be_t20, speech_harm_boundary__dignity_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(dignity_reading_be_t30, speech_harm_boundary__dignity_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(dignity_reading_be_t40, speech_harm_boundary__dignity_reading, base_extractiveness, 40, 0.66).
narrative_ontology:measurement(dignity_reading_be_t50, speech_harm_boundary__dignity_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(dignity_reading_su_t0, speech_harm_boundary__dignity_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(dignity_reading_su_t10, speech_harm_boundary__dignity_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(dignity_reading_su_t20, speech_harm_boundary__dignity_reading, suppression_requirement, 20, 0.64).
narrative_ontology:measurement(dignity_reading_su_t30, speech_harm_boundary__dignity_reading, suppression_requirement, 30, 0.69).
narrative_ontology:measurement(dignity_reading_su_t40, speech_harm_boundary__dignity_reading, suppression_requirement, 40, 0.73).
narrative_ontology:measurement(dignity_reading_su_t50, speech_harm_boundary__dignity_reading, suppression_requirement, 50, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_harm_boundary__dignity_reading, identity_coordination).
narrative_ontology:affects_constraint(speech_harm_boundary__dignity_reading, absolutist_reading).
narrative_ontology:affects_constraint(speech_harm_boundary__dignity_reading, harm_balancing_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'the speech-harm boundary' covers three structurally distinct constraints that must not share one story. The dignity_reading (this file) authors categorical exclusions with high epsilon borne by a defined speaker class; the harm_balancing_reading authors presumptive protection with case-by-case yield and a smaller, demonstrated-harm victim set; the absolutist_reading authors near-unconditional protection with negligible extraction. Each has its own epsilon, beneficiaries, victims, and enforcement form. Upstream/downstream structure: in European practice the dignity reading sits upstream of proportionality review — the categorical floor narrows the domain any balancing regime operates on — hence the influences edge to harm_balancing_reading; the absolutist reading runs as a parallel live position in other jurisdictions, coexisting without logical resolution. Cross-family contamination flows along these edges: scope-creep in this reading pressures the balancing sibling's operating environment, and absolutist jurisprudence supplies the standing critique that fuels this reading's resistance metric.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_harm_boundary__dignity_reading, powerful, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
