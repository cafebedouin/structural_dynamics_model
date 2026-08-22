% ============================================================================
% CONSTRAINT STORY: all_men_created_equal__textualist_paradox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_all_men_created_equal__textualist_paradox_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: all_men_created_equal__textualist_paradox_reading
 *   human_readable: Textualist-Paradox Reading of 'All Men Are Created Equal'
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the textualist-paradox reading of the 'all men
 *   are created equal' kernel: it argues that the universal grammar of the
 *   equality clause is structurally irreconcilable with any restricted
 *   18th-century-taxonomy application, and that this irreconcilability itself
 *   — independent of any positive universalist commitment — is enough to
 *   delegitimize originalist authority claims resting on the text's supposed
 *   coherence. This is a distinct constraint from the originalist_reading
 *   (which treats the restriction as the intended, authoritative scope) and
 *   from the universalist_reading (which treats equality as a substantive
 *   principle requiring expansion). All three share the same kernel text but
 *   read it into structurally different arrangements with different victims
 *   and different ε profiles: originalist_reading's victim set centers on
 *   those excluded by 18th-century taxonomy; universalist_reading's story
 *   centers on the practical work of expansion; this reading's victim is the
 *   originalist interpretive framework itself, whose authority claim the
 *   paradox exposes as internally incoherent.
 *
 * KEY AGENTS:
 *   - living_constitutionalist_advocates: primary beneficiary (organized/mobile) — gains interpretive leverage from the exposed contradiction
 *   - excluded_groups_seeking_inclusion: secondary beneficiary (moderate/constrained) — gains a textual lever without needing founder-intent concession
 *   - originalist_interpretive_framework: primary target (institutional/identity_locked) — its authority claim is the thing being delegitimized
 *   - founding_era_authority_claimants: secondary target (institutional/trapped) — legitimacy narratives built on textual coherence are exposed
 *   - universalist_advocates: excluded voice (organized/mobile) — a sibling reading not represented in this argument's internal logic
 *   - constitutional_courts: analytical observer (institutional/analytical) — adjudicates among competing kernel readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(all_men_created_equal__textualist_paradox_reading, 0.52).
domain_priors:suppression_score(all_men_created_equal__textualist_paradox_reading, 0.38).
domain_priors:theater_ratio(all_men_created_equal__textualist_paradox_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(all_men_created_equal__textualist_paradox_reading, tangled_rope).
narrative_ontology:human_readable(all_men_created_equal__textualist_paradox_reading, "Textualist-Paradox Reading of 'All Men Are Created Equal'").
narrative_ontology:topic_domain(all_men_created_equal__textualist_paradox_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(all_men_created_equal__textualist_paradox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(all_men_created_equal__textualist_paradox_reading, 'c4420d80-23fe-4ae8-875f-ea52db82f8da').
narrative_ontology:cs_kernel_codification('c4420d80-23fe-4ae8-875f-ea52db82f8da', fixed_text).
narrative_ontology:cs_authority_grounding('c4420d80-23fe-4ae8-875f-ea52db82f8da', lineage).
narrative_ontology:cs_interpretation_layer_present('c4420d80-23fe-4ae8-875f-ea52db82f8da').
narrative_ontology:cs_reading_relation('c4420d80-23fe-4ae8-875f-ea52db82f8da', all_men_created_equal__originalist_reading, forecloses).
narrative_ontology:cs_reading_relation('c4420d80-23fe-4ae8-875f-ea52db82f8da', all_men_created_equal__universalist_reading, influences).
narrative_ontology:cs_axiom('c4420d80-23fe-4ae8-875f-ea52db82f8da', foundational, universal_grammar_cannot_ground_bounded_scope).
narrative_ontology:cs_axiom_status(universal_grammar_cannot_ground_bounded_scope, holdable).
narrative_ontology:cs_axiom_grounding('c4420d80-23fe-4ae8-875f-ea52db82f8da', universal_grammar_cannot_ground_bounded_scope, conventional).
narrative_ontology:cs_axiom('c4420d80-23fe-4ae8-875f-ea52db82f8da', secondary, textual_incoherence_delegitimizes_authority_claim_independent_of_intent).
narrative_ontology:cs_axiom_status(textual_incoherence_delegitimizes_authority_claim_independent_of_intent, holdable).
narrative_ontology:cs_axiom_grounding('c4420d80-23fe-4ae8-875f-ea52db82f8da', textual_incoherence_delegitimizes_authority_claim_independent_of_intent, instrumental).
narrative_ontology:cs_reference_frame('c4420d80-23fe-4ae8-875f-ea52db82f8da', founding_text_universal_proclamation).
narrative_ontology:cs_drift_state('c4420d80-23fe-4ae8-875f-ea52db82f8da', contemporary_civil_rights_era_and_after, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c4420d80-23fe-4ae8-875f-ea52db82f8da', '').
narrative_ontology:cs_kernel_id(all_men_created_equal__textualist_paradox_reading, all_men_created_equal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(all_men_created_equal__textualist_paradox_reading, living_constitutionalist_advocates).
narrative_ontology:constraint_beneficiary(all_men_created_equal__textualist_paradox_reading, excluded_groups_seeking_inclusion).
narrative_ontology:constraint_victim(all_men_created_equal__textualist_paradox_reading, originalist_interpretive_framework).
narrative_ontology:constraint_victim(all_men_created_equal__textualist_paradox_reading, founding_era_authority_claimants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legal scholars, civil rights litigators, and jurists who argue that the universal grammar of 'all men are created equal' cannot be honestly restricted to its 18th-century social taxonomy without the text collapsing into self-contradiction. They use the textualist-paradox reading to license expansive application in litigation and legislative advocacy without having to defend a full universalist metaphysic — the contradiction itself does the delegitimizing work.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, living_constitutionalist_advocates, beneficiary,
    organized, generational, mobile, national).

% Historically excluded populations (enslaved people and their descendants, women, non-property-holders) whose claims to the equality language's benefit were structurally denied by originalist scope-limits. The paradox reading gives them a textual lever: the founders' own words indict the founders' own restriction, independent of whether the founders intended universal application.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, excluded_groups_seeking_inclusion, beneficiary,
    moderate, biographical, constrained, national).

% The interpretive tradition holding that founders' intent and 18th-century social categories fix the clause's scope. The textualist-paradox reading does not merely disagree with this tradition — it argues the tradition's own textual commitments are internally incoherent, since it must simultaneously assert the universal grammar is authoritative (to ground legitimacy) and non-universal in application (to preserve the restriction). This framework cannot easily absorb the challenge because its authority claim depends on the text meaning exactly what it says.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, originalist_interpretive_framework, payer,
    institutional, civilizational, identity_locked, national).

% Institutions and doctrines (originalist judicial reasoning, founder-veneration civic narratives) that derive legitimacy from treating the founding text as a coherent, intentional, boundedly-scoped document. The paradox reading strips this legitimacy by showing the document performatively contradicts itself — the institutions cannot exit this exposure because their authority is constituted by the very text now shown to be self-undermining.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, founding_era_authority_claimants, payer,
    institutional, civilizational, trapped, national).

% Hold that equality is a substantive universal principle requiring iterative expansion regardless of founder intent — a different reading of the same kernel. They are not part of THIS reading's argument (which rests on internal contradiction, not on affirming a positive universal principle) even though they often reach similar practical conclusions; their voice is absent from this reading's textual-paradox logic, which deliberately brackets metaphysical universalism to make a narrower, harder-to-deny claim.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, universalist_advocates, excluded,
    organized, generational, mobile, national).

% Adjudicate between competing readings of the equality clause across cases. They register the paradox argument as one available interpretive resource among several, weighing it against originalist and universalist claims when disposing of live controversies.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(all_men_created_equal__textualist_paradox_reading, living_constitutionalist_advocates).
narrative_ontology:fixing_cost_class(all_men_created_equal__textualist_paradox_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The paradox reading resolves an interpretive impasse without requiring courts or advocates to adopt a full metaphysical commitment to universal equality — it lets the text's own internal tension do the delegitimizing work, coordinating a broad coalition (textualists, critical legal scholars, civil rights advocates) who might otherwise disagree on foundational moral premises.
% TRANSFER_FUNCTION: Moves interpretive legitimacy away from originalist authority claims (which depend on the text being coherently bounded) toward expansive-application advocates (who benefit from the text being shown incoherent as bounded), without transferring any material resource directly — the transfer is argumentative and institutional standing, not money or labor.
% ABSENT_VOICES: Universalist advocates who affirm equality as a positive substantive principle are structurally absent from this reading's argument — the paradox reading brackets their metaphysical claim entirely, relying only on internal textual contradiction. They would object that the paradox reading under-claims: it destabilizes the originalist frame but does not itself commit to any expanded scope, leaving the door open to reactionary re-narrowing once the contradiction is 'resolved' by other means.
% DISAPPEARANCE_RATIONALE: If the textualist-paradox argument disappeared from constitutional discourse, originalist readings would face one less line of internal critique and could more comfortably assert textual coherence; excluded groups and expansionist advocates would lose a rhetorically potent, metaphysically economical tool and would need to fall back on purely universalist or purely originalist-revisionist arguments, both more contestable on their own terms.
% FOUNDING_PROBLEM: The clause was invoked to declare independence and ground a new political order in universal natural-rights language, while the same drafters and ratifying polity maintained chattel slavery, denied suffrage to women and the propertyless, and excluded non-white persons from full legal personhood — a gap between proclaimed universal grammar and enacted restricted practice that needed some interpretive account.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians outside both the originalist and living-constitutionalist camps (e.g., scholars of comparative constitutional drafting) attest that the gap between universal proclamation and restricted 18th-century practice is a documented historical fact, not an artifact of modern reading; abolitionist writers contemporaneous with the founding (outside the founding generation's own beneficiary class) also attested to the contradiction in real time, which corroborates the founding problem's liveness from outside the reading's present-day beneficiaries.
narrative_ontology:disappearance_verdict(all_men_created_equal__textualist_paradox_reading, world_rearranges).
narrative_ontology:founding_problem_status(all_men_created_equal__textualist_paradox_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(all_men_created_equal__textualist_paradox_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(all_men_created_equal__textualist_paradox_reading, 'none', 1).
narrative_ontology:epsilon_provenance(all_men_created_equal__textualist_paradox_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(all_men_created_equal__textualist_paradox_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(all_men_created_equal__textualist_paradox_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(all_men_created_equal__textualist_paradox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.52 — moderate, not high — because the extraction here is argumentative/institutional-legitimacy extraction, not material extraction: the paradox reading redistributes interpretive standing away from originalist authority without moving money, labor, or physical resources. Suppression is authored lower (0.38, and declining over the interval from 0.60 in 1776) because this reading does not depend on coercive enforcement to persist — it depends on the logical force of the contradiction being recognized, and recognition has become easier to sustain over time as documentary evidence of the founding-era gap has accumulated and as originalist frameworks have had to respond publicly rather than simply assert coherence by fiat. Theater ratio is moderate (0.44) reflecting that a meaningful share of originalist counter-argument is now performative reassertion of textual coherence rather than substantive rebuttal of the contradiction itself — the declining trajectory (0.55 to 0.44) shows that share shrinking somewhat as the contradiction becomes harder to perform past. Accessibility collapse is authored low (0.35) because alternative readings (originalist, universalist) remain fully live and contested — this is not a mountain-like collapse of alternatives. Resistance is authored high (0.71) because originalist-aligned institutions actively and vigorously resist the paradox framing, precisely because their authority claim is what is at stake.
 *
 * PERSPECTIVAL GAP:
 *   From the originalist_interpretive_framework's seat, this reading is pure attack: an attempt to use the text's own words to dissolve the authority claim the framework depends on, without offering any alternative textual discipline to replace it. From the living_constitutionalist_advocates' seat, the same argument is a coordination tool — it lets a broad coalition converge on delegitimizing narrow-scope readings without first winning an argument about substantive universal moral principles. The engine should compute these as different seat-classifications from the same structural data: a tangled_rope from the advocate seat (real coordination value plus a real cost imposed on originalist authority) and something closer to pure extraction from the originalist framework's own seat, since it bears the cost with no compensating coordination benefit it recognizes as legitimate.
 *
 * DIRECTIONALITY LOGIC:
 *   living_constitutionalist_advocates and excluded_groups_seeking_inclusion are declared beneficiaries because the paradox argument's persuasive force accrues directly to their interpretive and political projects — their d sits low. originalist_interpretive_framework and founding_era_authority_claimants are declared victims because the argument's entire operation is to strip their authority claim of textual coherence — their d sits high, and their exit options are authored as identity_locked/trapped because their institutional identity is constituted by the very textual-coherence claim under attack; abandoning the claim is not a move available within their own framework without becoming a different institution. universalist_advocates are excluded rather than beneficiary/victim because this specific reading brackets their metaphysical commitments entirely — they are structurally absent from the argument's internal logic even though they may benefit downstream.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading is not itself a mandatrophy case in the classic sense (an arrangement that outlived its founding function) — it is a genealogical exposure of another reading's (originalist_reading's) claim to have permanently and correctly resolved the founding problem. Classifying this as tangled_rope rather than snare matters: it preserves the fact that the argument does real coordination work (uniting otherwise-divided advocacy coalitions around a shared textual claim) even as it imposes a real, asymmetric cost on originalist institutional authority. Calling it a pure snare would erase the coordination function; calling it a rope would erase the real cost borne by originalist_interpretive_framework, which is not a strawman but a coherent, historically load-bearing interpretive tradition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    paradox_reading_kernel_relationship,
    'Is the textualist-paradox reading a genuinely distinct third reading of the equality kernel, or is it better understood as a meta-level critique that parasitically depends on the originalist reading existing to critique?',
    'Track whether the paradox argument retains independent force in a counterfactual discourse where originalist readings had never been dominant — if the argument''s persuasive structure requires originalist claims as its target, it may be more accurately modeled as a second-order constraint on originalist_reading rather than a coordinate sibling reading.',
    'If parasitic rather than coordinate, this story''s network edges should point primarily toward originalist_reading as an influences/forecloses relationship rather than being authored as a fully independent kernel reading with its own beneficiary/victim structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(paradox_reading_kernel_relationship, conceptual, 'Whether the paradox reading is an independent kernel reading or a parasitic critique of the originalist reading.').

omega_variable(
    contradiction_resolution_direction_underdetermined,
    'Does exposing the performative contradiction necessarily push resolution toward universalist expansion, or could it equally license a retreat to explicitly non-universal language (abandoning the equality clause''s universal grammar rather than expanding its application)?',
    'Historical and comparative analysis of how other polities have resolved analogous founding-text contradictions — some have narrowed proclamatory language rather than expanded application.',
    'If the contradiction is equally resolvable by narrowing language as by expanding application, the beneficiary declaration (living_constitutionalist_advocates, excluded_groups) is directionally contingent on a further political contest this reading alone does not settle — the paradox exposes instability without determining its resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contradiction_resolution_direction_underdetermined, conceptual, 'The paradox reading destabilizes the kernel without determining which direction resolution moves.').

omega_variable(
    framing_alternative_document_vs_legitimacy_claim,
    'Should this constraint be framed around the founding document''s text (the obvious framing) or around the legitimacy claim layered above it — the ongoing civic narrative that treats the founders as having spoken coherently and authoritatively?',
    'Compare classification outcomes under a document-framing (ε measured against textual content alone) versus a legitimacy-framing (ε measured against the institutional authority narrative the text is used to ground); check whether the two framings diverge in computed type.',
    'This story adopts the legitimacy-claim framing (the victim is the interpretive framework''s authority claim, not the physical text) because the document-framing alone would yield near-zero ε (a static 1776 text extracts nothing by itself) — the extraction is entirely in how the text is deployed as a legitimacy warrant. Adopting the document-only framing would misclassify this as near-mountain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_alternative_document_vs_legitimacy_claim, conceptual, 'Alternative framings (document text vs. legitimacy claim built on it) produce different classifications; this story deliberately adopts the legitimacy-claim framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(all_men_created_equal__textualist_paradox_reading, 1776, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(all__tr_t1776, all_men_created_equal__textualist_paradox_reading, theater_ratio, 1776, 0.55).
narrative_ontology:measurement(all__tr_t1863, all_men_created_equal__textualist_paradox_reading, theater_ratio, 1863, 0.5).
narrative_ontology:measurement(all__tr_t1920, all_men_created_equal__textualist_paradox_reading, theater_ratio, 1920, 0.48).
narrative_ontology:measurement(all__tr_t1954, all_men_created_equal__textualist_paradox_reading, theater_ratio, 1954, 0.46).
narrative_ontology:measurement(all__tr_t1970, all_men_created_equal__textualist_paradox_reading, theater_ratio, 1970, 0.45).
narrative_ontology:measurement(all__tr_t2000, all_men_created_equal__textualist_paradox_reading, theater_ratio, 2000, 0.44).
narrative_ontology:measurement(all__tr_t2026, all_men_created_equal__textualist_paradox_reading, theater_ratio, 2026, 0.44).

% Extraction over time
narrative_ontology:measurement(all__be_t1776, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 1776, 0.15).
narrative_ontology:measurement(all__be_t1863, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 1863, 0.28).
narrative_ontology:measurement(all__be_t1920, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 1920, 0.34).
narrative_ontology:measurement(all__be_t1954, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 1954, 0.41).
narrative_ontology:measurement(all__be_t1970, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 1970, 0.46).
narrative_ontology:measurement(all__be_t2000, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 2000, 0.49).
narrative_ontology:measurement(all__be_t2026, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 2026, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(all__su_t1776, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 1776, 0.6).
narrative_ontology:measurement(all__su_t1863, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 1863, 0.58).
narrative_ontology:measurement(all__su_t1920, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 1920, 0.52).
narrative_ontology:measurement(all__su_t1954, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 1954, 0.46).
narrative_ontology:measurement(all__su_t1970, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 1970, 0.42).
narrative_ontology:measurement(all__su_t2000, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 2000, 0.4).
narrative_ontology:measurement(all__su_t2026, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 2026, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(all_men_created_equal__textualist_paradox_reading, identity_coordination).
narrative_ontology:affects_constraint(all_men_created_equal__textualist_paradox_reading, originalist_reading).
narrative_ontology:affects_constraint(all_men_created_equal__textualist_paradox_reading, universalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the all_men_created_equal kernel. originalist_reading treats the founders' 18th-century social taxonomy as authoritative scope-fixing; universalist_reading treats equality as a substantive principle requiring iterative expansion independent of founder intent; this reading (textualist_paradox_reading) argues the universal grammar and restricted application are irreconcilable, which delegitimizes originalist_reading's coherence claim without itself affirming universalist_reading's substantive commitment. Each reading has its own ε, its own beneficiary/victim structure, and its own claimed type — they are linked via affects_constraints rather than merged into one story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
