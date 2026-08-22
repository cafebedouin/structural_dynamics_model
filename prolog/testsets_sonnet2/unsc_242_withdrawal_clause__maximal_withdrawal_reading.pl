% ============================================================================
% CONSTRAINT STORY: unsc_242_withdrawal_clause__maximal_withdrawal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unsc_242_withdrawal_clause__maximal_withdrawal_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: unsc_242_withdrawal_clause__maximal_withdrawal_reading
 *   human_readable: UNSC 242 Maximal Withdrawal Reading (French Definite Article, Full Retrocession)
 *   domain: international_law/diplomatic_history/treaty_interpretation
 *
 * SUMMARY:
 *   Security Council Resolution 242 (1967) called for 'withdrawal of Israeli
 *   armed forces from territories occupied in the recent conflict' alongside
 *   'termination of all claims or states of belligerency' and 'respect for
 *   territorial integrity and secure and recognized boundaries.' The English
 *   text omits a definite article before 'territories,' while the French text
 *   (equally authoritative under UN practice) uses 'des territoires occupes,'
 *   which many read as functionally definite. This story instantiates the
 *   MAXIMAL reading: withdrawal is mandatory from ALL occupied territories,
 *   grounded in the French definite-article construction and the Charter's
 *   Article 2(4) prohibition on acquisition of territory by force, understood
 *   as the resolution's controlling background norm. This is one of three
 *   sibling readings of the same kernel (unsc_242_withdrawal_clause): the
 *   partial_withdrawal_reading (discretionary scope, secure-boundaries
 *   carve-out) and the interpretive_authority_structure reading (which
 *   contests WHO has authority to resolve the ambiguity at all) are separate
 *   constraint stories with their own ε values, not alternative measurements
 *   of this one. Under this reading's own lights, ε is high (0.81) because
 *   the reading treats the obligation as comprehensive and non-discretionary
 *   — a mandatory, textually anchored withdrawal duty binding on the occupier
 *   regardless of negotiated outcome.
 *
 * KEY AGENTS:
 *   - dispossessed_palestinian_claimants: primary intended beneficiary (powerless/trapped) — holds enforceable legal position under this reading but no independent enforcement capacity
 *   - arab_state_territorial_claimants: institutional beneficiary (institutional/constrained) — invokes the reading diplomatically to demand return of occupied land
 *   - israeli_occupying_administration: primary bearer of the obligation (institutional/constrained) — rejects the reading's bindingness, cites drafting history
 *   - israeli_settler_population_in_occupied_territory: downstream payer (moderate/constrained) — presence incompatible with full retrocession
 *   - united_states_security_council_seat: structural gatekeeper (institutional/arbitrage) — avoids forcing adjudication, functionally excluding this reading from binding resolution
 *   - international_court_of_justice: analytical observer (analytical/analytical) — lends partial corroboration without directly adjudicating the textual dispute
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.81).
domain_priors:suppression_score(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.62).
domain_priors:theater_ratio(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unsc_242_withdrawal_clause__maximal_withdrawal_reading, rope).
narrative_ontology:human_readable(unsc_242_withdrawal_clause__maximal_withdrawal_reading, "UNSC 242 Maximal Withdrawal Reading (French Definite Article, Full Retrocession)").
narrative_ontology:topic_domain(unsc_242_withdrawal_clause__maximal_withdrawal_reading, "international_law/diplomatic_history/treaty_interpretation").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unsc_242_withdrawal_clause__maximal_withdrawal_reading, '1c99959e-9d85-4b37-a0e6-42f2ea6d8864').
narrative_ontology:cs_kernel_codification('1c99959e-9d85-4b37-a0e6-42f2ea6d8864', fixed_text).
narrative_ontology:cs_authority_grounding('1c99959e-9d85-4b37-a0e6-42f2ea6d8864', distributed).
narrative_ontology:cs_reading_relation('1c99959e-9d85-4b37-a0e6-42f2ea6d8864', unsc_242_withdrawal_clause__partial_withdrawal_reading, coexists_with).
narrative_ontology:cs_reading_relation('1c99959e-9d85-4b37-a0e6-42f2ea6d8864', unsc_242_withdrawal_clause__interpretive_authority_structure, influences).
narrative_ontology:cs_axiom('1c99959e-9d85-4b37-a0e6-42f2ea6d8864', foundational, french_definite_article_controls_scope).
narrative_ontology:cs_axiom_status(french_definite_article_controls_scope, holdable).
narrative_ontology:cs_axiom_grounding('1c99959e-9d85-4b37-a0e6-42f2ea6d8864', french_definite_article_controls_scope, conventional).
narrative_ontology:cs_axiom('1c99959e-9d85-4b37-a0e6-42f2ea6d8864', foundational, territorial_acquisition_by_force_categorically_barred).
narrative_ontology:cs_axiom_status(territorial_acquisition_by_force_categorically_barred, holdable).
narrative_ontology:cs_axiom_grounding('1c99959e-9d85-4b37-a0e6-42f2ea6d8864', territorial_acquisition_by_force_categorically_barred, deontological).
narrative_ontology:cs_reference_frame('1c99959e-9d85-4b37-a0e6-42f2ea6d8864', id_1967_ceasefire_territorial_status_quo_ante).
narrative_ontology:cs_drift_state('1c99959e-9d85-4b37-a0e6-42f2ea6d8864', post_oslo_and_post_2004_icj_opinion_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('1c99959e-9d85-4b37-a0e6-42f2ea6d8864', '').
narrative_ontology:cs_kernel_id(unsc_242_withdrawal_clause__maximal_withdrawal_reading, unsc_242_withdrawal_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__maximal_withdrawal_reading, dispossessed_palestinian_claimants).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__maximal_withdrawal_reading, arab_state_territorial_claimants).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__maximal_withdrawal_reading, un_general_assembly_bloc).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__maximal_withdrawal_reading, israeli_occupying_administration).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__maximal_withdrawal_reading, israeli_settler_population_in_occupied_territory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Have no state apparatus of their own to enforce the resolution and depend entirely on this reading being adopted by others with leverage. Under the maximal reading, they hold an enforceable legal position — full retrocession of occupied land is owed to them as a matter of the resolution's plain text — but they possess no independent means of compelling compliance and must rely on third-party diplomatic or coercive pressure on the occupier.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, dispossessed_palestinian_claimants, beneficiary,
    powerless, generational, trapped, regional).

% States with territory occupied in 1967 (Egypt, Syria, Jordan) invoke the definite-article reading to demand return of the Sinai, Golan, and West Bank/East Jerusalem as a matter of Charter obligation, not negotiated concession. Their leverage is diplomatic and coalition-based (Arab League, UN General Assembly voting blocs, Non-Aligned Movement) rather than military; they cannot compel withdrawal unilaterally and depend on the resolution's text carrying weight in third-party fora.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, arab_state_territorial_claimants, beneficiary,
    institutional, generational, constrained, regional).

% A recurring voting majority in the General Assembly and various UN bodies repeatedly reaffirms the maximal reading through non-binding resolutions, keeping the interpretation institutionally alive even though the Security Council itself has not adjudicated the textual dispute. This bloc sets the diplomatic agenda around the reading without holding enforcement power over the occupying state.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, un_general_assembly_bloc, beneficiary,
    organized, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__maximal_withdrawal_reading, un_general_assembly_bloc, agenda_setter).

% Administers the occupied territories and would, under the maximal reading, be obligated to withdraw from all of them including East Jerusalem, the West Bank, Golan, and (historically) Sinai and Gaza. It rejects the definite-article reading as authoritative, citing the drafting history's indefinite English text and the 'secure and recognized boundaries' clause of the same resolution. Its exit from the constraint is to contest its bindingness rather than comply; it retains military and administrative control regardless of the reading's adoption elsewhere.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, israeli_occupying_administration, payer,
    institutional, civilizational, constrained, regional).

% Communities established in the occupied territories after 1967 whose continued residence is directly incompatible with full retrocession under the maximal reading. They have organized politically to resist any withdrawal reading and depend on their state's rejection of the definite-article interpretation for their continued presence; a shift toward this reading's dominance would require their relocation.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, israeli_settler_population_in_occupied_territory, payer,
    moderate, biographical, constrained, local).

% Holds a permanent Security Council veto and has historically supported the partial/discretionary reading in diplomatic practice while never forcing a Council vote to formally adjudicate the textual ambiguity. Its structural position lets it avoid ever having to choose between the readings, which functionally excludes the maximal reading's beneficiaries from a forum that could bindingly resolve the dispute in their favor.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, united_states_security_council_seat, observer,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__maximal_withdrawal_reading, united_states_security_council_seat, excluded).

% Issued a 2004 advisory opinion on the wall in the occupied Palestinian territory that leaned toward the territorial-integrity default underlying the maximal reading, without directly adjudicating the French/English textual dispute of Resolution 242 itself. Its opinions carry persuasive but not enforceable weight, and it has not been asked to rule definitively on this specific textual question.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, international_court_of_justice, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unsc_242_withdrawal_clause__maximal_withdrawal_reading, diffuse).
narrative_ontology:fixing_cost_class(unsc_242_withdrawal_clause__maximal_withdrawal_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under this reading, the resolution coordinates a shared post-conflict settlement: the international community agrees a clear default rule (belligerent occupation does not confer title; territorial integrity is restored) so that territorial disputes are resolved by a fixed textual standard rather than by continued force of arms.
% TRANSFER_FUNCTION: Moves territorial sovereignty and administrative control from the occupying state back to the pre-1967 claimant states and populations, and moves diplomatic leverage from the occupier (who currently holds the land) to the claimants (who hold the textual claim under this reading).
% ABSENT_VOICES: The occupying state's own drafting-history evidence and the negotiators who insisted on the indefinite English article are treated by this reading as overridden by the French text and the Charter's Article 2(4) default; a full accounting of the English-language drafters' intent is structurally minimized within this reading rather than absent from the historical record.
% DISAPPEARANCE_RATIONALE: If the maximal reading vanished as a live diplomatic position, the Arab state claimants and Palestinian claimants would lose their strongest textual anchor for demanding full withdrawal as a matter of legal obligation rather than negotiated concession, and would fall back on the weaker partial-withdrawal framework or on non-242 legal instruments; the occupying administration's territorial position would be unaffected in the short term but its long-term diplomatic isolation on the settlement question would likely ease. Whether this counts as 'the world rearranging' or 'the world staying the same' is itself disputed between the reading's beneficiaries (who say the whole edifice of territorial-integrity claims rests on this text) and skeptics (who say state practice and military balance, not the resolution's grammar, actually determine outcomes).
% FOUNDING_PROBLEM: The founding problem was ending the 1967 war on terms that did not reward territorial conquest — establishing that occupation achieved by force could not be converted into permanent title, consistent with the UN Charter's post-1945 rejection of acquisition of territory by war.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars outside the Arab state coalition (including scholars sympathetic to Israel's security concerns) generally corroborate that the founding problem — preventing force-based territorial acquisition from becoming settled title — remains live and unresolved as a matter of customary international law; the ICJ's 2004 advisory opinion, issued by a body with no direct stake in either party's claim, lends outside corroboration to the continued vitality of the territorial-integrity default, though it stops short of adjudicating the specific French/English textual dispute this reading rests on.
narrative_ontology:disappearance_verdict(unsc_242_withdrawal_clause__maximal_withdrawal_reading, contested).
narrative_ontology:founding_problem_status(unsc_242_withdrawal_clause__maximal_withdrawal_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unsc_242_withdrawal_clause__maximal_withdrawal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unsc_242_withdrawal_clause__maximal_withdrawal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unsc_242_withdrawal_clause__maximal_withdrawal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.81) because, under this reading's own terms, the obligation is comprehensive and mandatory: no territory is exempted, no negotiated retention is permitted, and the occupier's continued administration of any occupied land constitutes an ongoing breach. Suppression is moderate-high (0.62): the reading does not depend on coercive enforcement machinery (there is no UN mechanism compelling compliance) but the occupier's persistent non-compliance requires sustained diplomatic and legal suppression of the competing partial reading to keep the maximal reading's claim alive in international fora. Theater ratio rises over the interval (0.20 to 0.40) as repeated non-binding General Assembly reaffirmations accumulate without corresponding withdrawal — a growing share of the reading's institutional life is symbolic reaffirmation rather than movement toward the substantive outcome. Accessibility collapse is authored low-moderate (0.35) because the competing partial reading remains fully available and actively asserted by the occupier and its allies — this is precisely the kernel-contest condition, not a case where alternatives have collapsed. Resistance is high (0.78): the reading meets sustained, well-resourced resistance from the occupying administration and its diplomatic allies, which is exactly what you'd expect from a contested reading rather than a settled natural-law-like constraint.
 *
 * PERSPECTIVAL GAP:
 *   From the claimant seats, this reading describes a rope: a coordination mechanism (post-war territorial settlement) that, once its terms are accepted, benefits the returning claimants without requiring ongoing coercion — the withdrawal obligation is self-executing once acknowledged. From the occupying administration's seat, the same textual claim reads as an attempted extraction: a disputed grammatical technicality (the French definite article) elevated into a comprehensive and irreversible territorial obligation that was never actually agreed to in the English-language negotiating record. The engine should compute these seats differently from the same structural data — that divergence is the point of authoring the reading rather than resolving it in prose.
 *
 * DIRECTIONALITY LOGIC:
 *   Dispossessed claimants and arab state claimants are declared beneficiaries: under this reading they hold the stronger legal claim, so directionality should derive toward the beneficiary end despite their limited enforcement power — the reading itself, if adopted, subsidizes their position even though they cannot compel it. The occupying administration and settler population are declared victims/payers: the reading, if binding, extracts territorial control and residence security from them. The occupying administration's power (institutional) is high but its exit option is only 'constrained' with respect to THIS reading specifically — it cannot exit the textual dispute, only contest the reading's authority, which is a different move than compliance-avoidance available to a true beneficiary.
 *
 * MANDATROPHY ANALYSIS:
 *   This is not a mandatrophy case in the classic sense (an arrangement that outlived its founding function) — the founding_problem_status is authored 'live' because the underlying norm (no title through conquest) remains actively contested rather than settled-and-abandoned. The risk this story guards against is a different failure: treating the maximal reading's ε=0.81 as evidence the RESOLUTION ITSELF is extractive, when in fact the resolution's TEXT is genuinely ambiguous and this is only one contested reading of it. The sibling files (partial_withdrawal_reading, interpretive_authority_structure) carry their own ε values under their own lights; none is more 'the resolution' than the others.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definite_article_controlling_authority,
    'When the English and French texts of a Security Council resolution diverge on a scope-determining grammatical feature, does the French definite construction control, or does the negotiating history behind the English indefinite construction control?',
    'A binding ICJ adjudication squarely on the textual dispute (rather than the 2004 advisory opinion''s oblique treatment), or a Security Council resolution explicitly clarifying scope, would resolve which language version controls as a matter of international treaty-interpretation doctrine (VCLT Article 33''s equal-authenticity rule does not itself resolve which reading prevails when the versions diverge in scope).',
    'If the French definite-article reading is confirmed as controlling, this reading''s claim to represent the resolution''s actual legal content strengthens substantially and the partial_withdrawal_reading''s structural position weakens correspondingly. If negotiating history is held controlling instead, this reading''s high ε (0.81) would be better understood as an aspirational or advocacy position rather than a description of settled legal obligation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(definite_article_controlling_authority, conceptual, 'Whether the French or English text controls the withdrawal clause''s scope is unresolved and may be irreducibly a matter of interpretive framework rather than fact.').

omega_variable(
    committer_kernel_disaggregation,
    'Is ''Resolution 242''s withdrawal clause'' one constraint with an ambiguous scope, or is the ambiguity itself evidence that no single constraint exists until an authoritative reading is chosen — making the interpretive_authority_structure sibling logically prior to both the maximal and partial readings?',
    'This is the committer structure documented per the Kernels and Readings framework: this file, partial_withdrawal_reading, and interpretive_authority_structure are three separate constraint stories linked via network.affects_constraints. Resolution would require either (a) a binding interpretive authority settling scope, collapsing the kernel into one reading, or (b) permanent multiplicity, in which case the kernel persists as an irreducibly contested object.',
    'If interpretive_authority_structure resolves in favor of a specific adjudicating body (e.g., binding ICJ jurisdiction), that body''s chosen reading becomes the operative constraint and the other two readings become historical positions rather than live contested constraints. Until then, all three readings remain simultaneously live, each with its own stakeholders and ε.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_kernel_disaggregation, conceptual, 'Documents the committer/kernel structure: this story is one reading among three, and the choice of reading is itself contested and prior to any single reading''s classification.').

omega_variable(
    settler_population_good_faith_reliance,
    'Does the passage of decades of settlement activity under a disputed legal claim generate an independent equitable claim for the settler population, separate from the underlying legality of the occupation itself?',
    'Comparative international law analysis of post-conflict population transfer and retrocession precedents (e.g., other territorial restitution cases) could establish whether prolonged de facto presence generates cognizable equitable interests distinct from sovereign title.',
    'If such equitable claims are recognized, the maximal reading''s characterization of the settler population as simple ''payers'' under a mandatory obligation would need qualification — full retrocession might require negotiated compensation or population-transfer provisions even under this reading, which would lower the reading''s clean ε somewhat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settler_population_good_faith_reliance, preference, 'Whether decades of settlement create independent equitable claims that complicate a purely mandatory-withdrawal reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unsc_tr_t1967, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 1967, 0.2).
narrative_ontology:measurement(unsc_tr_t1979, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 1979, 0.25).
narrative_ontology:measurement(unsc_tr_t1993, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 1993, 0.32).
narrative_ontology:measurement(unsc_tr_t2004, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 2004, 0.36).
narrative_ontology:measurement(unsc_tr_t2014, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 2014, 0.38).
narrative_ontology:measurement(unsc_tr_t2024, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(unsc_be_t1967, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 1967, 0.55).
narrative_ontology:measurement(unsc_be_t1979, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 1979, 0.6).
narrative_ontology:measurement(unsc_be_t1993, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 1993, 0.68).
narrative_ontology:measurement(unsc_be_t2004, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 2004, 0.74).
narrative_ontology:measurement(unsc_be_t2014, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 2014, 0.78).
narrative_ontology:measurement(unsc_be_t2024, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 2024, 0.81).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(unsc_242_withdrawal_clause__maximal_withdrawal_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unsc_242_withdrawal_clause__maximal_withdrawal_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.1).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__maximal_withdrawal_reading, unsc_242_withdrawal_clause__partial_withdrawal_reading).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__maximal_withdrawal_reading, unsc_242_withdrawal_clause__interpretive_authority_structure).

% DUAL FORMULATION NOTE:
% This story is one of three linked constraint files decomposing the natural-language label 'UNSC 242's withdrawal clause' per the epsilon-invariance principle: the maximal_withdrawal_reading (this file), the partial_withdrawal_reading (discretionary scope, ε lower, occupying-state beneficiary), and the interpretive_authority_structure (a prior-order dispute over who may authoritatively resolve the textual ambiguity at all). Each carries its own ε, beneficiary/victim structure, and classification; none is the 'true' reading of the resolution and none should be treated as a measurement of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unsc_242_withdrawal_clause__maximal_withdrawal_reading, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
