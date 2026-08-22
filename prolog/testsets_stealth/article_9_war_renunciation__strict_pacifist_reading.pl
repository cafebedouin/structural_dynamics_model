% ============================================================================
% CONSTRAINT STORY: article_9_war_renunciation__strict_pacifist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_9_war_renunciation__strict_pacifist_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: article_9_war_renunciation__strict_pacifist_reading
 *   human_readable: Article 9 Absolute War Renunciation (Strict Pacifist Reading)
 *   domain: constitutional law / security policy / institutional legitimacy
 *
 * SUMMARY:
 *   The strict pacifist reading instantiates the renunciation clause as a
 *   categorical rule: because the text declares that land, sea, and air
 *   forces 'shall never be maintained,' no organized military force of any
 *   kind — defensive included — is permissible, and the renunciation of war
 *   admits no exception. On this reading the postwar settlement binds the
 *   state to absolute demilitarization, with national defense routed
 *   exclusively through non-military means and dependence on the United
 *   States guarantee. The referent for the authored metrics is the standing
 *   arrangement this reading governs: eight decades of formal renunciation
 *   operating over a state that faces real regional threats, hosts a foreign
 *   protector's forces, and has progressively built armed institutions whose
 *   warrant this reading denies. The claim/metric gap is deliberate: the
 *   reading CLAIMS a categorical peace commitment (its adherents' framing),
 *   while the authored metrics describe a substantially extractive, actively
 *   enforced, increasingly ceremonial arrangement — the engine measures that
 *   divergence; nothing here reconciles them.
 *
 * KEY AGENTS:
 *   - japanese_state_security_autonomy: Primary target (institutional/trapped) — the state's capacity for independent armed defense is categorically foreclosed; recovery requires a supermajority amendment never approached.
 *   - japanese_citizens: Principal beneficiary with indirect cost-bearing (moderate/trapped) — eight decades without conscription or war deaths, purchased with dependence on a discretionary guarantor.
 *   - united_states_alliance_establishment: Concentrated beneficiary (institutional/arbitrage) — receives basing access, host-nation payments, and a regionally constrained ally.
 *   - pacifist_civil_society: Identity-fused beneficiary (organized/identity_locked) — civic identity constituted around the commitment; constitutes the public-opinion enforcement layer.
 *   - japanese_supreme_court: Agenda-setter (institutional/constrained) — controls whether the prohibition is ever adjudicated; has managed it through avoidance for eight decades.
 *   - jsdf_personnel: Bearing seat (organized/identity_locked) — serve an institution whose constitutional warrant this reading denies.
 *   - rearmament_advocates: Blocked seat (powerful/constrained) — pursue restored defense sovereignty through an amendment path they cannot complete.
 *   - east_asian_neighbor_states: External beneficiary (institutional/mobile) — receives the reassurance output without being governed by the order.
 *   - constitutional_scholars: Analytical observer (analytical/analytical) — divided along the unresolved semantic question, no material stake.
 *   - future_generations_citizens: Excluded seat (powerless/trapped) — bound by entrenchment without consent or seat.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_9_war_renunciation__strict_pacifist_reading, 0.63).
domain_priors:suppression_score(article_9_war_renunciation__strict_pacifist_reading, 0.68).
domain_priors:theater_ratio(article_9_war_renunciation__strict_pacifist_reading, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, extractiveness, 0.63).
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_9_war_renunciation__strict_pacifist_reading, tangled_rope).
narrative_ontology:human_readable(article_9_war_renunciation__strict_pacifist_reading, "Article 9 Absolute War Renunciation (Strict Pacifist Reading)").
narrative_ontology:topic_domain(article_9_war_renunciation__strict_pacifist_reading, "constitutional law / security policy / institutional legitimacy").

domain_priors:requires_active_enforcement(article_9_war_renunciation__strict_pacifist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_9_war_renunciation__strict_pacifist_reading, 'bb9f8769-059a-41dd-956b-a9e236ff9683').
narrative_ontology:cs_kernel_codification('bb9f8769-059a-41dd-956b-a9e236ff9683', fixed_text).
narrative_ontology:cs_authority_grounding('bb9f8769-059a-41dd-956b-a9e236ff9683', lineage).
narrative_ontology:cs_interpretation_layer_present('bb9f8769-059a-41dd-956b-a9e236ff9683').
narrative_ontology:cs_reading_relation('bb9f8769-059a-41dd-956b-a9e236ff9683', article_9_war_renunciation__inherent_right_reading, forecloses).
narrative_ontology:cs_reading_relation('bb9f8769-059a-41dd-956b-a9e236ff9683', article_9_war_renunciation__collective_self_defense_reading, forecloses).
narrative_ontology:cs_axiom('bb9f8769-059a-41dd-956b-a9e236ff9683', foundational, armed_forces_absolutely_impermissible).
narrative_ontology:cs_axiom_status(armed_forces_absolutely_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('bb9f8769-059a-41dd-956b-a9e236ff9683', armed_forces_absolutely_impermissible, conventional).
narrative_ontology:cs_axiom('bb9f8769-059a-41dd-956b-a9e236ff9683', foundational, defensive_war_equally_renounced).
narrative_ontology:cs_axiom_status(defensive_war_equally_renounced, holdable).
narrative_ontology:cs_axiom_grounding('bb9f8769-059a-41dd-956b-a9e236ff9683', defensive_war_equally_renounced, deontological).
narrative_ontology:cs_reference_frame('bb9f8769-059a-41dd-956b-a9e236ff9683', categorical_demilitarization_settlement).
narrative_ontology:cs_drift_state('bb9f8769-059a-41dd-956b-a9e236ff9683', post_reinterpretation_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('bb9f8769-059a-41dd-956b-a9e236ff9683', '').
narrative_ontology:cs_kernel_id(article_9_war_renunciation__strict_pacifist_reading, article_9_war_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, japanese_citizens).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, east_asian_neighbor_states).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, united_states_alliance_establishment).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, pacifist_civil_society).
narrative_ontology:constraint_victim(article_9_war_renunciation__strict_pacifist_reading, japanese_state_security_autonomy).
narrative_ontology:constraint_victim(article_9_war_renunciation__strict_pacifist_reading, rearmament_advocates).
narrative_ontology:constraint_victim(article_9_war_renunciation__strict_pacifist_reading, jsdf_personnel).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(article_9_war_renunciation__strict_pacifist_reading, japanese_citizens).
narrative_ontology:constraint_vindicates(article_9_war_renunciation__strict_pacifist_reading, yoshida_doctrine).
narrative_ontology:constraint_vindicates(article_9_war_renunciation__strict_pacifist_reading, absolute_pacifism_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Japanese state, considered as bearer of independent armed defense capacity. The renunciation text as strictly read removes organized military force from the state's menu permanently — no army, navy, or air arm may be maintained even for defense. National survival is routed through diplomacy, economic statecraft, and reliance on a foreign guarantor whose commitments are discretionary. Recovering the foregone option requires a two-thirds supermajority in both houses plus a referendum majority, a threshold never approached in eight decades.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, japanese_state_security_autonomy, payer,
    institutional, civilizational, trapped, national).

% Live under eight decades without conscription, without war deaths, and with military spending held to a fraction of peer levels by the political force of the renunciation commitment. They also pay indirectly: billions annually in host-nation support for the protector's bases, and exposure to a security environment they influence only through a guarantor they do not control. Citizenship offers no exit from the constitutional order either way.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, japanese_citizens, beneficiary,
    moderate, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(article_9_war_renunciation__strict_pacifist_reading, japanese_citizens, payer).

% Receives basing access across the archipelago, direct host-nation support payments, and an ally whose defense contribution is channeled into forms compatible with American regional strategy. The categorical ceiling on Japanese armed capacity keeps the ally dependent and the alliance asymmetric; the establishment shapes alliance terms and can adjust posture at will, bearing none of the constitutional restriction itself.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, united_states_alliance_establishment, beneficiary,
    institutional, generational, arbitrage, global).

% Organizations, movements, and the broad civic current for whom the renunciation clause is the core of postwar national identity — the 'peace nation' self-conception built after 1945. They mobilize at each revisionist push (most massively in 2015), litigate, and constitute the public-opinion enforcement layer. Their communal bonds, moral standing, and generational memory are constituted around the commitment's persistence; abandoning it is unthinkable from inside the identity.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, pacifist_civil_society, beneficiary,
    organized, generational, identity_locked, national).

% States in the region that suffered Japanese militarism before 1945 and have lived adjacent to a constitutionally demilitarized Japan since. The categorical removal of Japanese remilitarization reduces their defense planning burdens and supplies a standing reassurance signal. They are outside the constitutional order entirely: they receive the arrangement's outputs without being governed by it and can adjust their own postures freely.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, east_asian_neighbor_states, beneficiary,
    institutional, generational, mobile, continental).

% Holds sole authority to say definitively what the renunciation text permits, and has exercised it chiefly by declining: the leading decision affirmed the text's breadth while leaving the maintained forces' status undecided, and subsequent challenges have been dismissed on standing and ripeness grounds. Through this avoidance the Court manages the arrangement — deciding when the prohibition is enforced, absorbing drift into interpretation, and keeping the question formally open for eight decades.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, japanese_supreme_court, agenda_setter,
    institutional, generational, constrained, national).

% Roughly a quarter-million uniformed members serving an institution whose constitutional warrant the strict reading denies. They operate under mission restrictions, naming ambiguities (a 'Self-Defense Force' that is not legally a military), and a permanent constitutional cloud over the legitimacy of their service. Professional identity, pensions, and career structures are fused with the institution; leaving means abandoning the profession itself.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, jsdf_personnel, payer,
    organized, biographical, identity_locked, national).

% Political currents — dominant for much of the postwar period — committed to restoring ordinary defense sovereignty: a named military, unrestricted missions, alliance contribution on par with peers. They control legislative majorities repeatedly yet cannot complete the amendment their goal requires, so they work the interpretive margin instead: cabinet reinterpretations, incremental normalization statutes, budget milestones. Every gain is contested and partial; the categorical text remains above them.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, rearmament_advocates, payer,
    powerful, biographical, constrained, national).

% The academic discipline that has debated the text's force since 1947 — producing the doctrinal literature, the government's commissioned advisory panels, and the critiques of both. They hold no enforcement power and collect nothing from the arrangement; their seat is analytic, and their divisions track the unresolved semantic question rather than any material stake.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, constitutional_scholars, observer,
    analytical, biographical, analytical, national).

% People not yet born or not yet enfranchised who will inherit whatever settlement the current contest produces — bound by an entrenchment mechanism that requires supermajority consent of currently living voters to revisit. Their security environment may differ radically from the one in which the categorical commitment was adopted; they hold no seat in the amendment process that would let them adapt it.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, future_generations_citizens, excluded,
    powerless, civilizational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_9_war_renunciation__strict_pacifist_reading, united_states_alliance_establishment).
narrative_ontology:fixing_cost_class(article_9_war_renunciation__strict_pacifist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Replaces case-by-case security policymaking with a single categorical commitment: citizens coordinate on the expectation of never being conscripted or sent to war, neighboring states coordinate on the expectation of never facing Japanese armed forces, and the state coordinates its entire postwar strategy around economy-first development under an external guarantee. One rule solves, once and durably, what ad hoc policy would relitigate every year.
% TRANSFER_FUNCTION: Moves the defense function from the Japanese state to the United States guarantee and to diplomatic instruments; moves fiscal resources away from military expenditure toward civilian uses; moves the option of organized violence permanently off the state's menu; and, in the standing arrangement, moves status and legal-clarity costs onto the uniformed personnel of an institution whose warrant is denied.
% ABSENT_VOICES: Future generations bound by an entrenchment they cannot revisit without the supermajority consent of current voters; Okinawan communities hosting the foreign bases that an absolute-renunciation settlement struggles to justify; regional governments whose security depends on which way the unresolved semantic contest settles. None holds a seat in the amendment process.
% DISAPPEARANCE_RATIONALE: If the categorical prohibition vanished overnight, the state would move immediately to normalize defense institutions — budgets toward peer levels, mission restrictions lifted, the alliance renegotiated from dependence toward contribution. Neighboring states would respond with their own buildups, the host-nation payment stream would be repriced, and the postwar identity settlement around which pacifist civil society is organized would rupture. Nearly every named seat's situation rearranges.
% FOUNDING_PROBLEM: Built to solve the problem of a defeated militarist empire: ensuring that the state whose armed forces had dominated its domestic politics, waged aggressive war across Asia, and led its own population to devastation could never again maintain the forces or wage the wars that made that history possible.
% FOUNDING_PROBLEM_CORROBORATION: The original problem's content is corroborated from outside any benefiting party by the Tokyo tribunal record, occupation-era government documents, and the contemporaneous testimony of the regional states that suffered the militarism in question. Its status is disputed: constitutional historians outside the pacifist movement broadly attest that the original problem — recurrence of Imperial-style militarism — is extinct under the transformed institutions, while the claim that a generalized version remains live is attested almost exclusively by the arrangement's beneficiaries (pacifist civil society and allied opposition parties). No corroborating source outside the beneficiary set attests the generalized liveness, and that absence is itself signal.
narrative_ontology:disappearance_verdict(article_9_war_renunciation__strict_pacifist_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_9_war_renunciation__strict_pacifist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_9_war_renunciation__strict_pacifist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_9_war_renunciation__strict_pacifist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_9_war_renunciation__strict_pacifist_reading, 0.63, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_9_war_renunciation__strict_pacifist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_9_war_renunciation__strict_pacifist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_9_war_renunciation__strict_pacifist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.63) is a U-shape over the interval: near-total foreclosure of the state's defense option at the start (adopted amid a hot war on the peninsula), damped through the middle decades as the external guarantee compensated the surrendered option, then rising again in the recent era as the arrangement's continuing formal force extracts new costs — interpretive contortion from the state, status limbo from uniformed personnel, and a quietly repudiated promise from the adherents who supplied its enforcement. Suppression (0.68) is the arrangement's defining feature and is authored as a raw structural property — the engine, not this story, scales extractiveness by directionality and scope; suppression itself is unscaled. It reflects entrenchment (an amendment threshold never approached) plus judicial avoidance, not physical coercion. Accessibility collapse (0.52) is partial by design: once the categorical reading is granted, the military-defense branch of the option space closes completely, but non-military security paths (diplomacy, alliance, neutrality) remain open. Resistance (0.78) is among the highest recorded for any constitutional norm — eight decades of continuous revisionist pressure from legislatively dominant forces. Theater ratio (0.46) rises monotonically: the arrangement began as raw functional prohibition and has migrated toward identity performance (anniversary ritual, commemorative politics, mobilization-as-expression) as practice diverges from the text. The suppression_requirement series traces one full enforcement cycle: hot mobilization at the start (peaking around the 1960 security-treaty crisis), a long consensus trough through the high-growth decades, remobilization against the reinterpretation-era pushes (peaking at the 2015 legislation), and early fatigue decay since. The cycle's driver is the alternation of revisionist pushes and pacifist countermobilization; each settlement ratifies a partial fait accompli, so the oscillation functions as a gradual-release ratchet rather than mere noise — and for the adherents it operates as intermittent reinforcement (hope at each mobilization, disappointment at each settlement), which is itself part of what sustains participation. All three series are authored on one shared nine-point grid; the scalar base_properties values are the interval-end states, measured in the post-buildup phase.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats should compute differently, and the divergence is structural rather than dispositional. From the state-security seat the arrangement is the permanent foreclosure of a sovereign function, compensated by a guarantee it does not control; from the citizen seat it is eight decades of conscription-free life at modest fiscal cost; from the alliance-establishment seat it is a cheapened regional architecture it did not pay for constitutionally. Same-level lateral differentiation runs through the citizen and advocate seats: both are members of the same polity, but the advocates hold legislative power and a (blocked) amendment path while the citizens hold only opinion and vote, so identical nominal standing yields different exits and different experienced arrangements. Inter-institutionally, the court experiences the text as a management problem (when to decide, when to avoid), the executive as a constraint to be worked by interpretation, and the external establishment as someone else's constitution that happens to organize its ally. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low directionalities: citizens (broad diffuse gains, some indirect payment), neighbor states (pure external recipients, mobile exit), the alliance establishment (concentrated recipient with arbitrage-grade control of terms), and pacifist civil society (identity-level gains, no exit desire). Victim declarations map to high directionalities: the state-security seat bears the categorical foreclosure with trapped exit; rearmament advocates bear the blocked-preference cost with a constrained path; uniformed personnel bear status and legal-clarity costs with professionally fused exit. The court seat declares neither benefit nor burden — it administers — and collects no rents from the arrangement's operation. Because the largest concentrated gains accrue to a seat outside the governing jurisdiction entirely, the arrangement's extraction lands asymmetrically: the governed pay in autonomy and identity, an ungoverned seat collects in basing, payments, and strategic latitude.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — recurrence of Imperial-style militarism — is credibly extinct, which is the classic precondition for mistaking a surviving arrangement for a dead one. The classification resists both directions of mislabeling. Reading it as pure coordination (rope) would erase the real asymmetric extraction: the state seat's surrendered autonomy, the personnel's status limbo, and the externally captured gains. Reading it as pure extraction (snare) would erase the genuine coordination core: the conscription-free decades, the regional reassurance function, and the fiscal diversion that no ad hoc policy replicated. The tangled-rope classification holds both truths and lets the temporal data carry the diagnostic weight: rising theater against a contested founding problem is the drift signature toward inertial maintenance. The identity-lock dynamics sharpen the forecast — the arrangement's enforcement now rests substantially on a generational identity fusion (pacifist civil society) and a professional fusion (uniformed personnel) rather than on institutional machinery; if either identity frame breaks under generational turnover, enforcement capacity collapses faster than the entrenchment alone would predict, and the arrangement completes the drift toward performance without function. Whether that break occurs is precisely what the suppression-mechanism omega tracks.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_location,
    'Which semantic force does the renunciation text carry — a categorical prohibition on maintaining any armed forces whatsoever, or a prohibition scoped to offensive war potential that leaves minimum defensive capacity permissible?',
    'Drafting-history evidence (the Ashida amendment records and occupation-era government documents), a court holding squarely addressing the constitutionality of maintained forces rather than dismissing on justiciability grounds, or formal constitutional amendment settling the text''s force.',
    'Resolved categorical: this constraint stands as authored, with state security autonomy in the victim set. Resolved scoped: this reading collapses into the inherent-right sibling''s constraint, the victim set shrinks to aggressive remilitarization only, and the measured suppression drops sharply.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Location of the kernel contest: the semantic force of ''never be maintained'' and whether ''war potential'' comprehends defensive capability.').

omega_variable(
    sibling_reading_structural_delta,
    'How would this constraint''s structure change if instantiated under a sibling reading of the same kernel?',
    'Compile the sibling stories (inherent_right_reading, collective_self_defense_reading) and compare victim sets, epsilon, and enforcement profiles against this file.',
    'Under the inherent-right sibling, state security autonomy exits the victim set and extraction over the state seat falls; under the collective sibling, a new beneficiary set (allied states) enters and scope widens. This story''s classification is conditional on the strict instantiation and should not be aggregated across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Committer routing: this constraint is the strict_pacifist_reading of kernel article_9_war_renunciation; each sibling instantiates a different constraint with its own epsilon and party structure.').

omega_variable(
    alliance_dependence_reliability,
    'Does the United States security guarantee remain a reliable substitute for the foreclosed defense option across the constraint''s operating horizon?',
    'Alliance-commitment indicators: host-nation support trajectories, United States force-posture decisions, and observed crisis-response behavior in regional contingencies.',
    'If the guarantee wavers, the constraint''s cost side comes to dominate — defenselessness without a functioning substitute — and the classification drifts toward pure extraction; if the guarantee holds firm, the arrangement remains a mixed bargain with a genuine coordination core.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alliance_dependence_reliability, empirical, 'Exogenous stability of the alliance-dependence route through which this reading routes national defense.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the constraint''s current suppression structural (amendment entrenchment, judicial avoidance) or internalized (pacifist identity fusion sustaining the norm after formal enforcement capacity decayed)?',
    'Post-erosion opinion trajectory: if pacifist majorities persist as enforcement capacity decays and the wartime-generation cohort turns over, the internalized component dominates; if opinion tracks enforcement capacity, the structural component dominates.',
    'Internalized suppression means the arrangement outlives its enforcement machinery — raising persistence estimates and the drift-toward-inertia risk; purely structural suppression predicts comparatively rapid collapse once entrenchment erodes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Mechanism split of the measured suppression between entrenchment structure and identity fusion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_9_war_renunciation__strict_pacifist_reading, 0, 79).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(art9_strict_tr_t0, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(art9_strict_tr_t0, observed).
narrative_ontology:measurement(art9_strict_tr_t10, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement_basis(art9_strict_tr_t10, observed).
narrative_ontology:measurement(art9_strict_tr_t20, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement_basis(art9_strict_tr_t20, observed).
narrative_ontology:measurement(art9_strict_tr_t30, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement_basis(art9_strict_tr_t30, observed).
narrative_ontology:measurement(art9_strict_tr_t40, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 40, 0.25).
narrative_ontology:measurement_basis(art9_strict_tr_t40, observed).
narrative_ontology:measurement(art9_strict_tr_t50, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 50, 0.3).
narrative_ontology:measurement_basis(art9_strict_tr_t50, observed).
narrative_ontology:measurement(art9_strict_tr_t60, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 60, 0.34).
narrative_ontology:measurement_basis(art9_strict_tr_t60, observed).
narrative_ontology:measurement(art9_strict_tr_t70, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 70, 0.41).
narrative_ontology:measurement_basis(art9_strict_tr_t70, observed).
narrative_ontology:measurement(art9_strict_tr_t79, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 79, 0.46).
narrative_ontology:measurement_basis(art9_strict_tr_t79, observed).

% Extraction over time
narrative_ontology:measurement(art9_strict_be_t0, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 0, 0.66).
narrative_ontology:measurement_basis(art9_strict_be_t0, observed).
narrative_ontology:measurement(art9_strict_be_t10, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement_basis(art9_strict_be_t10, observed).
narrative_ontology:measurement(art9_strict_be_t20, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement_basis(art9_strict_be_t20, observed).
narrative_ontology:measurement(art9_strict_be_t30, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement_basis(art9_strict_be_t30, observed).
narrative_ontology:measurement(art9_strict_be_t40, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 40, 0.54).
narrative_ontology:measurement_basis(art9_strict_be_t40, observed).
narrative_ontology:measurement(art9_strict_be_t50, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 50, 0.56).
narrative_ontology:measurement_basis(art9_strict_be_t50, observed).
narrative_ontology:measurement(art9_strict_be_t60, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 60, 0.59).
narrative_ontology:measurement_basis(art9_strict_be_t60, observed).
narrative_ontology:measurement(art9_strict_be_t70, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 70, 0.62).
narrative_ontology:measurement_basis(art9_strict_be_t70, observed).
narrative_ontology:measurement(art9_strict_be_t79, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 79, 0.63).
narrative_ontology:measurement_basis(art9_strict_be_t79, observed).

% Suppression requirement over time
narrative_ontology:measurement(art9_strict_su_t0, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement_basis(art9_strict_su_t0, observed).
narrative_ontology:measurement(art9_strict_su_t10, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 10, 0.75).
narrative_ontology:measurement_basis(art9_strict_su_t10, observed).
narrative_ontology:measurement(art9_strict_su_t20, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement_basis(art9_strict_su_t20, observed).
narrative_ontology:measurement(art9_strict_su_t30, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 30, 0.48).
narrative_ontology:measurement_basis(art9_strict_su_t30, observed).
narrative_ontology:measurement(art9_strict_su_t40, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 40, 0.45).
narrative_ontology:measurement_basis(art9_strict_su_t40, observed).
narrative_ontology:measurement(art9_strict_su_t50, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 50, 0.52).
narrative_ontology:measurement_basis(art9_strict_su_t50, observed).
narrative_ontology:measurement(art9_strict_su_t60, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 60, 0.6).
narrative_ontology:measurement_basis(art9_strict_su_t60, observed).
narrative_ontology:measurement(art9_strict_su_t70, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 70, 0.74).
narrative_ontology:measurement_basis(art9_strict_su_t70, observed).
narrative_ontology:measurement(art9_strict_su_t79, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 79, 0.68).
narrative_ontology:measurement_basis(art9_strict_su_t79, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_9_war_renunciation__strict_pacifist_reading, identity_coordination).
narrative_ontology:affects_constraint(article_9_war_renunciation__strict_pacifist_reading, article_9_war_renunciation__inherent_right_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__strict_pacifist_reading, article_9_war_renunciation__collective_self_defense_reading).

% DUAL FORMULATION NOTE:
% The colloquial object 'Article 9' decomposes into three structurally distinct constraints sharing one kernel (the renunciation text). This file instantiates the strict pacifist pole: categorical prohibition, victim set including state security autonomy, suppression carried by entrenchment. The inherent-right sibling instantiates a scoped prohibition (offensive war potential only) — its victim set excludes state security autonomy and its extraction profile concentrates elsewhere. The collective-self-defense sibling extends the inherent-right line to allied defense, widening scope and adding beneficiary seats. Upstream/downstream: the inherent-right reading is the administratively dominant line from which the collective reading extends; the strict reading is the textual-originalist pole from which both depart, and each sibling's legitimacy argument is framed against the strict reading's categorical claim. Epsilon differs across the family because the readings differ in what they forbid — they are different constraints, not one constraint viewed from angles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
