% ============================================================================
% CONSTRAINT STORY: article_9_war_renunciation__inherent_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_9_war_renunciation__inherent_right_reading, []).

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
 *   constraint_id: article_9_war_renunciation__inherent_right_reading
 *   human_readable: Article 9 Inherent-Right Reading: Self-Administered Minimum-Necessary Defense Threshold
 *   domain: constitutional_law/security_policy/institutional_legitimacy
 *
 * SUMMARY:
 *   Japan's postwar constitution renounces war in Article 9. The arrangement
 *   modeled here is the government's inherent-right reading, dominant from
 *   the early 1950s to the 2014 cabinet reinterpretation: the renunciation is
 *   scoped to aggressive war, self-defense is held to be an inherent
 *   sovereign right, and armed forces are lawful insofar as they remain the
 *   'minimum necessary' for defense. Under this reading the text operates as
 *   a self-administered proportionality threshold — the Self-Defense Forces
 *   are organizationally legitimate but scope-limited — rather than a
 *   prohibition. The colloquial label 'Article 9' decomposes into three
 *   structurally distinct claims (categorical prohibition, self-administered
 *   threshold, collective-defense authorization) with different extraction
 *   profiles and different victim sets; per the epsilon-invariance principle
 *   they are authored as separate stories in one kernel family, linked by
 *   network edges. Claim and metrics are authored independently: the
 *   arrangement is claimed here as tangled_rope — a genuine
 *   commitment-and-reassurance coordination function carrying asymmetric
 *   extraction — while the metrics describe its actual operation at interval
 *   end; the engine computes per-seat classifications from the structural
 *   data, and any divergence between claim and computed type is the
 *   measurement the corpus exists to take.
 *
 * KEY AGENTS:
 *   - japanese_cabinet_government: agenda-setting executive and primary beneficiary (institutional/constrained) — authors the reading, sets the threshold's content, collects interpretive authority
 *   - cabinet_legislation_bureau: enforcement interpreter (institutional/constrained) — policed the minimum-necessary line until overridden in 2014
 *   - jsdf_defense_establishment: dual-positioned beneficiary/payer (institutional/constrained) — exists lawfully under the reading, capped by it
 *   - jsdf_service_members: target seat (moderate/constrained) — bear contested constitutional status and narrowed use-of-force rules
 *   - pacifist_constitutional_constituency: target seat (organized/identity_locked) — text maintained in form, content set elsewhere
 *   - constitutional_scholarship_community: target seat (organized/constrained) — interpretive authority displaced with no institutional channel
 *   - us_japan_alliance_establishment: secondary beneficiary (institutional/constrained) — the cap underwrites the alliance division of labor
 *   - regional_neighbor_states: external beneficiaries (institutional/mobile) — receive reassurance they cannot verify or enforce
 *   - japanese_supreme_court: abstaining observer (institutional/analytical) — its non-decision is load-bearing
 *   - opposition_constitutional_parties: excluded seat (organized/constrained) — bypassed by cabinet reinterpretation without a prior Diet vote
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_9_war_renunciation__inherent_right_reading, 0.7).
domain_priors:suppression_score(article_9_war_renunciation__inherent_right_reading, 0.75).
domain_priors:theater_ratio(article_9_war_renunciation__inherent_right_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_9_war_renunciation__inherent_right_reading, tangled_rope).
narrative_ontology:human_readable(article_9_war_renunciation__inherent_right_reading, "Article 9 Inherent-Right Reading: Self-Administered Minimum-Necessary Defense Threshold").
narrative_ontology:topic_domain(article_9_war_renunciation__inherent_right_reading, "constitutional_law/security_policy/institutional_legitimacy").

domain_priors:requires_active_enforcement(article_9_war_renunciation__inherent_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_9_war_renunciation__inherent_right_reading, '8999fd6f-4b11-430e-bacc-d66cd41f118f').
narrative_ontology:cs_kernel_codification('8999fd6f-4b11-430e-bacc-d66cd41f118f', fixed_text).
narrative_ontology:cs_authority_grounding('8999fd6f-4b11-430e-bacc-d66cd41f118f', extraction).
narrative_ontology:cs_interpretation_layer_present('8999fd6f-4b11-430e-bacc-d66cd41f118f').
narrative_ontology:cs_reading_relation('8999fd6f-4b11-430e-bacc-d66cd41f118f', article_9_war_renunciation__strict_pacifist_reading, forecloses).
narrative_ontology:cs_reading_relation('8999fd6f-4b11-430e-bacc-d66cd41f118f', article_9_war_renunciation__collective_self_defense_reading, influences).
narrative_ontology:cs_axiom('8999fd6f-4b11-430e-bacc-d66cd41f118f', foundational, inherent_sovereign_self_defense_right).
narrative_ontology:cs_axiom_status(inherent_sovereign_self_defense_right, holdable).
narrative_ontology:cs_axiom_grounding('8999fd6f-4b11-430e-bacc-d66cd41f118f', inherent_sovereign_self_defense_right, deontological).
narrative_ontology:cs_axiom('8999fd6f-4b11-430e-bacc-d66cd41f118f', foundational, minimum_necessary_proportionality_limit).
narrative_ontology:cs_axiom_status(minimum_necessary_proportionality_limit, holdable).
narrative_ontology:cs_axiom_grounding('8999fd6f-4b11-430e-bacc-d66cd41f118f', minimum_necessary_proportionality_limit, instrumental).
narrative_ontology:cs_reference_frame('8999fd6f-4b11-430e-bacc-d66cd41f118f', minimum_necessary_defense_threshold).
narrative_ontology:cs_drift_state('8999fd6f-4b11-430e-bacc-d66cd41f118f', post_2014_cabinet_reinterpretation, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('8999fd6f-4b11-430e-bacc-d66cd41f118f', '').
narrative_ontology:cs_kernel_id(article_9_war_renunciation__inherent_right_reading, article_9_war_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, japanese_cabinet_government).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, jsdf_defense_establishment).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, us_japan_alliance_establishment).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, regional_neighbor_states).
narrative_ontology:constraint_victim(article_9_war_renunciation__inherent_right_reading, pacifist_constitutional_constituency).
narrative_ontology:constraint_victim(article_9_war_renunciation__inherent_right_reading, constitutional_scholarship_community).
narrative_ontology:constraint_victim(article_9_war_renunciation__inherent_right_reading, jsdf_service_members).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, cabinet_legislation_bureau).
narrative_ontology:constraint_victim(article_9_war_renunciation__inherent_right_reading, jsdf_defense_establishment).
narrative_ontology:constraint_vindicates(article_9_war_renunciation__inherent_right_reading, inherent_sovereign_self_defense_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds executive office under a constitution whose Article 9 renounces war. Since the early 1950s it has authored and maintained the official interpretation that the renunciation covers aggressive war while self-defense remains an inherent sovereign right, allowing it to build and operate armed forces under a pacifist text. It sets the content of 'minimum necessary' through cabinet decisions and the Cabinet Legislation Bureau's advisory opinions. Formal amendment requires a two-thirds Diet supermajority plus a national referendum it has never assembled; changing course in practice has meant reinterpretation, as in 2014.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, japanese_cabinet_government, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(article_9_war_renunciation__inherent_right_reading, japanese_cabinet_government, beneficiary).

% A small bureau of career bureaucrats inside the Cabinet Office that drafts the authoritative opinions on what the constitution permits. For decades its memos policed the line between permitted defensive capacity and prohibited war-making, and its sign-off was the practical gate every defense plan had to pass. It gained institutional weight as the guardian of the interpretive line; in 2014 the cabinet overrode its long-standing position on collective self-defense, showing that its gatekeeping held only so long as the cabinet deferred to it.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, cabinet_legislation_bureau, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(article_9_war_renunciation__inherent_right_reading, cabinet_legislation_bureau, beneficiary).

% The Ministry of Defense, the Self-Defense Forces as an organization, and the defense industry. The reading gives them lawful existence: under a text forbidding the maintenance of 'war potential,' they operate as forces limited to defense. The same reading caps them — procurement, posture, and operations must be justified as minimum necessary, and every expansion request is argued against that yardstick. Budget, missions, and legal protections are all set inside that argument.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, jsdf_defense_establishment, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(article_9_war_renunciation__inherent_right_reading, jsdf_defense_establishment, payer).

% Roughly a quarter million uniformed personnel serving in forces whose constitutional footing is contested. Their use-of-force rules are narrower than allied militaries', their legal status was settled only gradually after decades of litigation over their position as 'special public servants,' and they carry the political exposure of serving under a text many fellow citizens read as forbidding their existence. Leaving the service means leaving the career they trained for; the status ambiguity is not something an individual exit resolves.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, jsdf_service_members, payer,
    moderate, biographical, constrained, national).

% Citizens, religious organizations, unions, and civic groups for whom the renunciation of war is a constitutive political identity — the postwar guarantee that Japan will not again fight a war of choice, maintain conscription, or export arms. They mobilized by the millions against the 1960 Security Treaty revision and against the 2015 security legislation. The text they identify with is maintained in form while its operative content is set by cabinet interpretation they do not control; leaving the arrangement would mean abandoning the commitment it is named after.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, pacifist_constitutional_constituency, payer,
    organized, generational, identity_locked, national).

% Academic constitutional lawyers, Japan Federation of Bar Associations committees, and the scholarly societies that historically claimed authority over the text's meaning. The large majority held that the Self-Defense Forces were constitutional only under the minimum-necessary reading and that the 2014 collective-self-defense decision was unconstitutional. Their interpretive authority was displaced in practice by the Cabinet Legislation Bureau and then by cabinet decision; they can publish dissent and testify, but no institutional channel converts their readings into law.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, constitutional_scholarship_community, payer,
    organized, biographical, constrained, national).

% The US Pacific command structure and the alliance bureaucracy. Japan's self-imposed cap underwrites the alliance's division of labor: the United States supplies deterrence and power projection; Japan supplies bases, host-nation funding, and a military that will not act independently or offensively. The cap makes Japan a predictable partner and answers the fear of an independent Japanese military that shaped occupation policy. The alliance can press for burden-sharing increases but has an interest in the cap's formal survival.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, us_japan_alliance_establishment, beneficiary,
    institutional, generational, constrained, global).

% China, the two Koreas, and Southeast Asian states whose threat assessments of Japan are lowered by the renunciation framework and the minimum-necessary cap. They receive reassurance they did not bargain for and cannot enforce: the cap is self-interpreted in Tokyo, and their formal objection channels — bilateral statements, United Nations positions — carry no domestic legal force in Japan. Their benefit depends on a line they do not control.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, regional_neighbor_states, beneficiary,
    institutional, generational, mobile, continental).

% The judiciary that could adjudicate the Self-Defense Forces' constitutionality and has consistently declined: standing doctrines dismissed the Sunakawa challenge in 1959, the Naganuma missile-site ruling was reversed on procedural grounds in 1973, and no merits decision on the forces' constitutionality has ever issued. Its abstention is the load-bearing silence in the arrangement — the cabinet's interpretive monopoly holds partly because no competing authoritative interpreter exists.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, japanese_supreme_court, observer,
    institutional, generational, analytical, national).

% The Japan Socialist Party and Communist Party through the Cold War, and the later Democratic Party of Japan: parties that contested the government's reading in the Diet, demanded judicial referral, and advanced their own readings (strict prohibition or United Nations-centered collective security). The 2014 reinterpretation was adopted by cabinet decision without a prior Diet vote, so their constitutional objections never reached a binding forum; their leverage was confined to post-hoc legislation fights and elections.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__inherent_right_reading, opposition_constitutional_parties, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_9_war_renunciation__inherent_right_reading, japanese_cabinet_government).
narrative_ontology:fixing_cost_class(article_9_war_renunciation__inherent_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a commitment problem: how a state whose prior regime waged aggressive war credibly binds itself to defensive-only force. The renunciation reassures neighbors and the domestic public; the minimum-necessary threshold coordinates expectations about what Japan's forces will and will not do; the settlement underwrites the alliance division of labor in which the United States provides deterrence and Japan provides bases and fiscal support without an independent offensive capability.
% TRANSFER_FUNCTION: Moves interpretive authority over the constitution's operative meaning from the amendment process and the courts to the cabinet; moves fiscal resources and legal existence to the defense establishment inside a cabinet-defined envelope; moves reassurance to neighbor states and the alliance at the cost of the pacifist constituency's textual claim and the service members' legal clarity.
% ABSENT_VOICES: Regional neighbor states receive the reassurance but have no seat in setting the threshold they depend on; Article 96 referendum voters were bypassed when the 2014 change was adopted by cabinet decision rather than amendment; strict-pacifist scholars and opposition parties stood outside the interpretive process that displaced their readings; Self-Defense Forces rank-and-file have no voice in the rules governing their own status.
% DISAPPEARANCE_RATIONALE: If the threshold and its interpretive machinery vanished overnight — Japan announcing no self-imposed limit on force — regional threat perceptions would reprice within months, the alliance's division-of-labor bargain would reopen, domestic pacifist politics would confront an unconstrained military, and the diplomatic asset of the peace constitution would evaporate. The arrangement's dependents are numerous enough that its removal rearranges the regional security architecture rather than leaving the world as it was.
% FOUNDING_PROBLEM: Post-1945 Japan needed to re-enter international society without reviving the militarism that produced the Pacific War, and — once occupation forces began withdrawing in 1950 — needed a lawful basis for any defensive force at all under a text forbidding the maintenance of 'war potential.' The inherent-right reading was the settlement: keep the renunciation, claim self-defense as an inherent sovereign right, cap forces at the minimum necessary for defense.
% FOUNDING_PROBLEM_CORROBORATION: The cabinet and the defense establishment attest the settlement as live law. Outside the benefiting parties: the constitutional scholarship community and the Japan Federation of Bar Associations have repeatedly held the 2014 displacement of the threshold unconstitutional, attesting that the founding settlement's content was moved without consent; pre-2014 Cabinet Legislation Bureau memos (1972, 1981) attest the stricter line the cabinet later overrode; majority public-opposition polling at the 2014-2015 votes corroborates that the settlement no longer commanded the consent its founding claimed. No corroborating source outside the beneficiary set attests the settlement as unmodified live law.
narrative_ontology:disappearance_verdict(article_9_war_renunciation__inherent_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_9_war_renunciation__inherent_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_9_war_renunciation__inherent_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_9_war_renunciation__inherent_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_9_war_renunciation__inherent_right_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_9_war_renunciation__inherent_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_9_war_renunciation__inherent_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_9_war_renunciation__inherent_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is 0.70 at interval end because the threshold's content is set unilaterally by the cabinet: 'minimum necessary' has no external referee, and the gap between the renunciation text and the force it governs widened across six decades (police reserve to blue-water fleet) without any consent event. Suppression is 0.75 because persistence depends on actively maintained machinery — Cabinet Legislation Bureau gatekeeping, Diet schedule management, judicial abstention, protest policing at the 1960 and 2015 peaks — not on participant preference. Theater is 0.55 because the text's ceremonial function (diplomatic asset, identity symbol) now outweighs its operative binding. Accessibility collapse is 0.45: alternatives remain reachable but costly — the Article 96 amendment gate has never cleared its double threshold and courts decline jurisdiction, yet the 2014 reinterpretation proves the alternative path was live. Resistance is 0.70: mass mobilization at both ends of the interval (the 1960 Security Treaty protests, the 2015 legislation rallies), near-unanimous scholarly opposition to the 2014 change, and bar-association challenges. All three series share one eight-point grid (1950-2014) so every metric is authored at every examined time point. The suppression_requirement series is tracked because this story specifically traces enforcement-capacity change: a deliberate U-shape (founding-era enforcement demand against mass protest, a normalization trough in the 1970s-80s when the reading became settled doctrine, renewed demand as expansion pressure met bureau resistance through the 2000s). The suppression mechanism is overwhelmingly structural — forum denial, interpretive monopoly, a closed amendment gate — rather than internalized: the suppressed seats know precisely what has happened and say so.
 *
 * PERSPECTIVAL GAP:
 *   The cabinet seat experiences a lawful settlement it administers and profits from: the text preserved as diplomatic asset, the forces legitimated, the threshold movable by decision. The pacifist constituency seat experiences a hollowed text: the guarantee it identifies with is maintained in form while its content is set in a room it cannot enter. The JSDF establishment seat experiences both existence and cap — lawful being, bounded doing. Neighbor states receive reassurance they cannot verify; the scholarship community holds a reading with no institutional channel. Same-level actors diverge on constraint-specific factors rather than power: the cabinet and the Cabinet Legislation Bureau are both institutional seats, but the cabinet sets the threshold while the bureau policed it and was overridden when the two diverged — agenda control and exit options, not nominal standing, differentiate them. For the pacifist constituency the identity lock is ideological: the renunciation is not a policy they hold but an identity they are; exit would dissolve the constitutive commitment, which is why their mode is resistance rather than exit. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: the cabinet (collects interpretive authority and flexibility — near the beneficiary end), the US alliance establishment (the cap underwrites the division of labor), neighbor states (reassurance), and the JSDF establishment (lawful existence). Victims: the pacifist constituency (text hollowed, identity-locked — near the target end), the scholarship community (authority displaced, no exit from dissent), and JSDF service members (contested status, narrowed rules, constrained exit). The JSDF establishment is genuinely dual-positioned — it gains being and pays boundedness — and because the directionality override surface is keyed by power atom rather than by agent, no override is authored: an institutional-wide override would misapply to the cabinet and the alliance seats; the duality is carried instead by its dual role declaration and this commentary. Neighbor states sit at the beneficiary end despite holding no seat in the arrangement: their directionality is low because the structure subsidizes them, not because they control it — directionality encodes structural subsidy, not agenda power.
 *
 * MANDATROPHY ANALYSIS:
 *   Two mislabelings are prevented. Reading the arrangement as pure coordination — 'the peace constitution held for seventy years' — misses the extraction: the threshold's content moved by cabinet decision, the interpretive monopoly displaced courts and scholarship, and the founding settlement's consent was never re-sought. Reading it as pure extraction — 'the pacifist text is cover for remilitarization' — misses the real coordination good: the renunciation held, no conscription was restored, neighbors' threat assessments stayed lower than a normal Japanese military would command, and the alliance division of labor persisted. The tangled_rope classification holds both: the same threshold that reassures is the one whose content the cabinet monopolizes. The R5 interview sharpens the picture: the founding problem (legitimating defensive forces under a renunciation text) was solved by this reading, but the arrangement persisted past its settlement while its content kept moving — the contested founding_problem_status combined with the world_rearranges disappearance verdict flags the capture/zombie pattern for cross-check against the theater_ratio series.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This story instantiates the inherent-right reading of the article_9_war_renunciation kernel; would adopting a sibling reading change the victim set, the extraction structure, or the classification?',
    'Author and classify the sibling stories separately (strict_pacifist_reading, collective_self_defense_reading) and compare per-seat classifications across the kernel family; the 2014 cabinet decision is a natural experiment in which the collective reading displaced this reading''s threshold line.',
    'Under the strict reading the entire defense establishment joins the victim set and extractiveness rises sharply; under the collective reading the threshold widens and the extraction envelope grows. This story''s classification holds only for its own threshold structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Kernel-level ambiguity: which reading governs determines who pays and how much.').

omega_variable(
    minimum_necessary_indeterminacy,
    'Does the ''minimum necessary'' threshold actually bind procurement and posture, or does the cabinet''s monopoly on interpreting it convert the threshold into self-certification that launders expansion?',
    'Compare Japanese force trajectories against independent defensive-need benchmarks (allied force structures for comparable territory and threat environment); track cases where the Cabinet Legislation Bureau rejected or reshaped procurement on threshold grounds versus cases where the cabinet overrode the line.',
    'If the threshold binds, part of the measured extractiveness is the price of the reassurance good and the arrangement moves toward pure coordination; if it launders, the threshold is cover and the arrangement moves toward pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minimum_necessary_indeterminacy, empirical, 'Whether the proportionality line constrains the defense establishment or certifies its expansion.').

omega_variable(
    judicial_abstention_load_bearing,
    'Is the arrangement''s stability dependent on the courts'' refusal to adjudicate the Self-Defense Forces'' constitutionality — would a merits ruling collapse the cabinet''s interpretive monopoly?',
    'Counterfactual analysis of the Sunakawa (1959) and Naganuma (1973) litigation lines: had standing been found and a merits ruling issued, trace whether the bureau''s gatekeeping and the cabinet''s interpretive monopoly could survive a competing authoritative interpreter.',
    'If abstention is load-bearing, part of the measured suppression is manufactured by forum denial rather than consent, raising effective suppression; if a ruling would likely have ratified the reading, suppression sits closer to settled consensus.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_abstention_load_bearing, empirical, 'Whether judicial abstention is structural support for the interpretive monopoly.').

omega_variable(
    alliance_subsidy_dependence,
    'Is the threshold''s political sustainability subsidized by the US security umbrella — does the cap hold only while deterrence is trusted, with domestic breach pressure rising as the perceived umbrella weakens?',
    'Time-series correlation between perceived US commitment (alliance crises, burden-sharing disputes, regional threat growth after 2010) and domestic expansion pressure (defense budget shares, reinterpretation drives, referendum polling on Article 9 amendment).',
    'If subsidized, the threshold is contingent on an external arrangement and persistence analysis must model the alliance as a co-structure; if not, the cap reflects a durable domestic settlement in its own right.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alliance_subsidy_dependence, empirical, 'Whether the cap''s durability rides on external deterrence rather than domestic commitment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_9_war_renunciation__inherent_right_reading, 1950, 2014).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1950, article_9_war_renunciation__inherent_right_reading, theater_ratio, 1950, 0.2).
narrative_ontology:measurement_basis(arti_tr_t1950, observed).
narrative_ontology:measurement(arti_tr_t1960, article_9_war_renunciation__inherent_right_reading, theater_ratio, 1960, 0.28).
narrative_ontology:measurement_basis(arti_tr_t1960, observed).
narrative_ontology:measurement(arti_tr_t1970, article_9_war_renunciation__inherent_right_reading, theater_ratio, 1970, 0.38).
narrative_ontology:measurement_basis(arti_tr_t1970, observed).
narrative_ontology:measurement(arti_tr_t1980, article_9_war_renunciation__inherent_right_reading, theater_ratio, 1980, 0.44).
narrative_ontology:measurement_basis(arti_tr_t1980, observed).
narrative_ontology:measurement(arti_tr_t1990, article_9_war_renunciation__inherent_right_reading, theater_ratio, 1990, 0.47).
narrative_ontology:measurement_basis(arti_tr_t1990, observed).
narrative_ontology:measurement(arti_tr_t2000, article_9_war_renunciation__inherent_right_reading, theater_ratio, 2000, 0.5).
narrative_ontology:measurement_basis(arti_tr_t2000, observed).
narrative_ontology:measurement(arti_tr_t2005, article_9_war_renunciation__inherent_right_reading, theater_ratio, 2005, 0.52).
narrative_ontology:measurement_basis(arti_tr_t2005, observed).
narrative_ontology:measurement(arti_tr_t2014, article_9_war_renunciation__inherent_right_reading, theater_ratio, 2014, 0.55).
narrative_ontology:measurement_basis(arti_tr_t2014, observed).

% Extraction over time
narrative_ontology:measurement(arti_be_t1950, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 1950, 0.35).
narrative_ontology:measurement_basis(arti_be_t1950, observed).
narrative_ontology:measurement(arti_be_t1960, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 1960, 0.44).
narrative_ontology:measurement_basis(arti_be_t1960, observed).
narrative_ontology:measurement(arti_be_t1970, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 1970, 0.52).
narrative_ontology:measurement_basis(arti_be_t1970, observed).
narrative_ontology:measurement(arti_be_t1980, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 1980, 0.56).
narrative_ontology:measurement_basis(arti_be_t1980, observed).
narrative_ontology:measurement(arti_be_t1990, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 1990, 0.58).
narrative_ontology:measurement_basis(arti_be_t1990, observed).
narrative_ontology:measurement(arti_be_t2000, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement_basis(arti_be_t2000, observed).
narrative_ontology:measurement(arti_be_t2005, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 2005, 0.66).
narrative_ontology:measurement_basis(arti_be_t2005, observed).
narrative_ontology:measurement(arti_be_t2014, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 2014, 0.7).
narrative_ontology:measurement_basis(arti_be_t2014, observed).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1950, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 1950, 0.75).
narrative_ontology:measurement_basis(arti_su_t1950, observed).
narrative_ontology:measurement(arti_su_t1960, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 1960, 0.8).
narrative_ontology:measurement_basis(arti_su_t1960, observed).
narrative_ontology:measurement(arti_su_t1970, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 1970, 0.55).
narrative_ontology:measurement_basis(arti_su_t1970, observed).
narrative_ontology:measurement(arti_su_t1980, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 1980, 0.45).
narrative_ontology:measurement_basis(arti_su_t1980, observed).
narrative_ontology:measurement(arti_su_t1990, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 1990, 0.5).
narrative_ontology:measurement_basis(arti_su_t1990, observed).
narrative_ontology:measurement(arti_su_t2000, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement_basis(arti_su_t2000, observed).
narrative_ontology:measurement(arti_su_t2005, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 2005, 0.68).
narrative_ontology:measurement_basis(arti_su_t2005, observed).
narrative_ontology:measurement(arti_su_t2014, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 2014, 0.75).
narrative_ontology:measurement_basis(arti_su_t2014, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_9_war_renunciation__inherent_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_9_war_renunciation__inherent_right_reading, strict_pacifist_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__inherent_right_reading, collective_self_defense_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Article 9' covers three structurally distinct constraints: a categorical prohibition (strict_pacifist_reading, under which the entire defense establishment is unconstitutional), a self-administered proportionality threshold (this reading, which extracts interpretive authority while capping the establishment), and a collective-defense authorization (collective_self_defense_reading, which widens the threshold's envelope). Their epsilon values differ by a wide margin and their victim sets are disjoint in part. The inherent-right reading is upstream of the collective reading: it created the lawful force and the inherent-right premise that the 2014 reinterpretation leveraged. The stories are linked through network edges rather than merged, because a single story with a variable threshold would violate epsilon invariance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
