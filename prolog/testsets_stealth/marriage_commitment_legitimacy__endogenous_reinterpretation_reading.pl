% ============================================================================
% CONSTRAINT STORY: marriage_commitment_legitimacy__endogenous_reinterpretation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_legitimacy__endogenous_reinterpretation_reading, []).

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
 *   constraint_id: marriage_commitment_legitimacy__endogenous_reinterpretation_reading
 *   human_readable: Manifesto-as-Revelation: Endogenous Reversal Reading of the Marriage Covenant (1890-1930)
 *   domain: religious institutional history/political theology/commitment systems
 *
 * SUMMARY:
 *   Within the Church's post-1890 authority structure, members are bound to
 *   receive the 1890 Manifesto — the declaration suspending new plural
 *   marriages — as genuine prophetic revelation: God commanded the reversal
 *   to preserve the Church for its higher purposes. The standing arrangement
 *   under contest is the interpretive and disciplinary apparatus that
 *   maintains this account: the canonized text, the Second Manifesto's
 *   hearings and prosecutions, temple-recommend discipline, and the teaching
 *   framework that recasts monogamy as a new covenant stage while retaining
 *   the earlier marriage revelation as scripture. On this reading's own
 *   lights the arrangement is benign: obedience is covenant duty, the costs
 *   borne by plural families and dissenters are sacrifice, and the Church's
 *   survival vindicates the command. The authored metrics register what the
 *   structure actually does — real enforcement, costs concentrated on
 *   specific seats, rising performative maintenance as the founding crisis
 *   recedes — and the engine computes each seat's classification from that
 *   data.
 *
 * KEY AGENTS:
 *   - - prophetic_officeholders: Agenda-setter and primary beneficiary (institutional/identity_locked) — administers the reading; legitimacy accrues to their office
 *   - - lds_church_membership: Beneficiary with secondary payer position (organized/identity_locked) — receives covenant continuity; bears sacrifice and the discipline of kin
 *   - - plural_family_members: Primary target (powerless/trapped) — bears the reversal's concentrated costs
 *   - - manifest_dissenters: Secondary target (moderate/constrained) — severed for refusing the reading
 *   - - federal_enforcement_state: Excluded party (institutional/mobile) — its coercion is the fact the reading's causal claim explains away
 *   - - church_history_scholars: Analytical observer (analytical/analytical) — sees the full structure, collects nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.35).
domain_priors:suppression_score(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.6).
domain_priors:theater_ratio(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, "Manifesto-as-Revelation: Endogenous Reversal Reading of the Marriage Covenant (1890-1930)").
narrative_ontology:topic_domain(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, "religious institutional history/political theology/commitment systems").

domain_priors:requires_active_enforcement(marriage_commitment_legitimacy__endogenous_reinterpretation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, '2877fdf6-ddac-4271-9154-afddc01fa55c').
narrative_ontology:cs_kernel_codification('2877fdf6-ddac-4271-9154-afddc01fa55c', fixed_text).
narrative_ontology:cs_authority_grounding('2877fdf6-ddac-4271-9154-afddc01fa55c', lineage).
narrative_ontology:cs_interpretation_layer_present('2877fdf6-ddac-4271-9154-afddc01fa55c').
narrative_ontology:cs_reading_relation('2877fdf6-ddac-4271-9154-afddc01fa55c', marriage_commitment_legitimacy__exogenous_override_reading, forecloses).
narrative_ontology:cs_reading_relation('2877fdf6-ddac-4271-9154-afddc01fa55c', marriage_commitment_legitimacy__hybrid_pragmatic_reading, coexists_with).
narrative_ontology:cs_axiom('2877fdf6-ddac-4271-9154-afddc01fa55c', foundational, manifesto_is_binding_divine_command).
narrative_ontology:cs_axiom_status(manifesto_is_binding_divine_command, holdable).
narrative_ontology:cs_axiom_grounding('2877fdf6-ddac-4271-9154-afddc01fa55c', manifesto_is_binding_divine_command, theological).
narrative_ontology:cs_axiom('2877fdf6-ddac-4271-9154-afddc01fa55c', secondary, monogamy_is_new_covenant_stage).
narrative_ontology:cs_axiom_status(monogamy_is_new_covenant_stage, holdable).
narrative_ontology:cs_axiom_grounding('2877fdf6-ddac-4271-9154-afddc01fa55c', monogamy_is_new_covenant_stage, theological).
narrative_ontology:cs_reference_frame('2877fdf6-ddac-4271-9154-afddc01fa55c', living_prophet_covenant_continuity).
narrative_ontology:cs_drift_state('2877fdf6-ddac-4271-9154-afddc01fa55c', contemporary_historiographic_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2877fdf6-ddac-4271-9154-afddc01fa55c', '').
narrative_ontology:cs_kernel_id(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, marriage_commitment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, prophetic_officeholders).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, lds_church_membership).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, plural_family_members).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, manifest_dissenters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, lds_church_membership).
narrative_ontology:constraint_vindicates(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, living_prophet_continuing_revelation).
narrative_ontology:constraint_vindicates(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, divine_providential_preservation_of_church).
narrative_ontology:constraint_vindicates(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, covenant_continuity_across_dispensations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The First Presidency and Quorum of the Twelve administer the reading: they issued the Manifesto (1890), canonized it as scripture (1908), issued the Second Manifesto with hearings under oath and prosecutions (1904), and rule on scope questions about where the suspension binds. The reading constitutes their office: if the Manifesto were capitulation rather than revelation, the claim that living prophets receive binding revelation — the foundation of succession authority — would break. Holding office and rejecting the reading are not simultaneously available to them.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, prophetic_officeholders, agenda_setter,
    institutional, generational, identity_locked, global).

% Rank-and-file Saints received the reversal as covenant guidance: the Church survived, temples stayed open, and the community's belonging, ritual life, and gathering continued. They also bore the sacrifice: cancelled expectations of plural marriage, the interpretive work of reconciling the earlier marriage revelation with the suspension, and the discipline of neighbors and kin who dissented. Two generations of persecution had fused membership with identity; leaving meant losing family, economy, and perceived salvation at once.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, lds_church_membership, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, lds_church_membership, payer).

% Women and children in existing plural households, and those who entered plural marriages between 1890 and 1904 believing the practice would continue, carried the reversal's concentrated costs: husbands prosecuted and imprisoned, marriages entered in good faith later treated as grounds for loss of temple privileges and fellowship, social standing downgraded, and — for the post-1904 cohort — excommunication. Their position inside the covenant community left no exit that did not also cost family, property, and salvation as they understood it.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, plural_family_members, payer,
    powerless, biographical, trapped, regional).

% Members who read the Manifesto as capitulation — including two apostles dropped from the Quorum in 1905-1906 and the fundamentalist networks that formed around rival claims of authority — refused the reading and were severed: resignation under pressure, loss of fellowship, excommunication. Their exit was real but costly: they left the community, its economy, and its temple promises, and built or joined schismatic groups that preserved the plural-marriage practice the reading suspended. They never coalesced into an effective bloc inside the institution; severance and geography kept them divided.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, manifest_dissenters, payer,
    moderate, biographical, constrained, regional).

% Congress, the federal courts, and the anti-polygamy enforcement apparatus drove the crisis: disincorporation, confiscation of temples and property, disenfranchisement, and imprisonment of plural husbands. Their account — that the Manifesto was capitulation to enforcement — sits outside the reading's legitimacy framework, which recasts their pressure as catalyst rather than cause. They are not bound by the constraint, could withdraw enforcement at will, and are the voice the reading's causal claim exists to explain away.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, federal_enforcement_state, excluded,
    institutional, generational, mobile, national).

% Historians inside and outside the tradition assess the causal question from journals, correspondence, and the enforcement timeline. Their seat sees the full structure: the revelation account's late documentation, the coercion crescendo that preceded the announcement, and the interpretive work the reading performs. They collect nothing from the arrangement and are disciplined by nothing; their testimony is the corroboration surface the reading cannot supply for itself.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, church_history_scholars, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, prophetic_officeholders).
narrative_ontology:fixing_cost_class(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The reading solves a collective-action problem inside a persecuted covenant community: it gives every member one account of the reversal that preserves institutional continuity, keeps temples and ordinances operating, and holds the community's boundary against schism — coordination achieved by attributing the reversal to God rather than to the state.
% TRANSFER_FUNCTION: Moves interpretive assent and covenant sacrifice from the membership — and, at the margin, fellowship and salvation-as-understood from dissenters and post-Manifesto plural families — to the prophetic office, as legitimacy-capital demonstrating that living revelation continues to govern.
% ABSENT_VOICES: The federal enforcement state and the dissenters it created are outside the legitimacy conversation: prosecutors would testify the Manifesto was capitulation; excommunicated fundamentalists would testify the reversal betrayed the covenant. Both voices are excluded by the reading's own causal claim — the account that would have to answer them is the account that silences them.
% DISAPPEARANCE_RATIONALE: If members stopped receiving the Manifesto as revelation overnight, the Church's authority claim — that living prophets receive binding revelation — would face immediate crisis; the fundamentalist position (the Manifesto was man-made) would gain force; and the covenant framework reconciling the earlier marriage revelation with monogamous practice would collapse into open doctrinal contradiction. Succession legitimacy, temple discipline, and the community boundary all hang on the reading.
% FOUNDING_PROBLEM: The Church faced legal extinction under federal anti-polygamy enforcement: disincorporation, confiscation of temples and property, disenfranchisement of members, and imprisonment of leaders. The Manifesto ended new plural marriages to save the institutional Church.
% FOUNDING_PROBLEM_CORROBORATION: Federal enforcement records, the terms of the Edmunds-Tucker Act, and the Reed Smoot hearing transcript attest the existential threat from outside the benefiting parties; the scholarly historiography corroborates that the Manifesto was issued under compulsion — corroborating the founding problem while disputing the endogenous causal attribution. No source outside the benefiting parties attests the revelation account itself.
narrative_ontology:disappearance_verdict(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.35, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_legitimacy__endogenous_reinterpretation_reading_tests).
:- end_tests(marriage_commitment_legitimacy__endogenous_reinterpretation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored low-to-moderate (0.35 at interval end) because the reading itself sacralizes the costs it concentrates: the losses of plural families and the severance of dissenters are framed as covenant sacrifice rather than taking, while the broad membership nets continuity, temple access, and belonging. Suppression (0.60) is a raw structural property, authored unscaled — the Second Manifesto's oath and prosecutions, the removal of two apostles, the excommunication wave of 1910-1911 — but it is not totalizing: private doubt was tolerated, and the boundary targeted practice and public denial of prophetic authority rather than inner belief; the suppression_requirement series traces the enforcement machinery's build-up (0.35 to 0.75 by 1911) and its normalization-decay thereafter, which is why this story tracks enforcement capacity explicitly. Theater (0.45) rises monotonically: as the legal crisis recedes, an increasing share of maintenance is commemorative retelling and narrative curation ('catalyst, not cause') rather than crisis response. Accessibility_collapse (0.55): alternatives did not vanish — fundamentalist schisms and the exogenous account survived outside the frame — but inside the frame they collapse once prophetic authority is sustained, since one cannot coherently sustain the prophet and reject his declaration. Resistance (0.45): continued secret marriages, the dropped apostles, and the fundamentalist networks mounted sustained resistance without ever coalescing — the payer seats were severed, scattered, and identity-divided, which is why their structural power stayed low. The claim (tangled_rope) and the metrics are independent authored facts: the reading's own lights would say rope, but the structure requires active enforcement and concentrates real costs on named seats — the hybrid signature.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from one structure. From the agenda-setter seat (officeholders, fused with the office the reading constitutes), the arrangement is continuity itself — the demonstration that living revelation governs. From the membership seat (identity-locked, net beneficiary), it is sacrifice accepted for the community's sake. From the plural-family seat (no exit that preserves family, property, and salvation together), the same structure operated as the cancellation of lived covenants. From the dissenter seat it was betrayal administered as discipline; from the excluded federal seat it was capitulation. The engine derives these divergences from the structural data; this story does not adjudicate which seat sees truly — that adjudication is the kernel contest recorded in the omegas.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real flows: the prophetic office receives the arrangement's principal yield — legitimacy-capital, since Manifesto-as-revelation demonstrates the living-prophet claim on which succession authority rests — and the membership receives covenant continuity, institutional survival, and belonging while paying interpretive labor and the discipline of kin. Victim declarations map to concentrated costs: plural-family members bore prosecutions, marriages voided in effect, and excommunication, with exits that sacrificed family, property, and salvation simultaneously; dissenters bore severance for refusing the reading. The federal enforcement state sits outside the frame by the reading's own design — its pressure is the fact the causal claim explains away — and historians observe from a seat that collects nothing. Scope is national-to-continental: at that scale the causal claim ('catalyst, not cause') is expensive to verify, which is precisely what lets the endogenous account hold against the enforcement timeline.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — legal extinction under federal enforcement — is dead: statehood (1896), the return of confiscated property, and the Smoot settlement (1907) resolved it, and the practice had already ended in fact. The arrangement persists and now does different work: demonstrating living prophetic authority and holding the boundary against the fundamentalist position. Authored honestly, founding_problem_status=dead with disappearance_verdict=world_rearranges is exactly the mismatch the battery flags — the arrangement is maintained past its function, and the theater series (0.20 to 0.45) tracks that migration from crisis response to commemorative maintenance. This classification prevents two mislabels: pure extraction (the coordination function — continuity, covenant framework, communal survival — was and remains real and valued by the net-beneficiary majority) and pure coordination (the costs are concentrated on identifiable seats and enforced against refusal, not voluntarily assumed). If the reframing is tacit doctrinal revision rather than continuity (see the theological_continuity omega), the maintenance drifts toward the inertial pattern — a suspended doctrine kept canon by retelling.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of the kernel marriage_commitment_legitimacy — the endogenous reading (the Manifesto as genuine divine command). Would the exogenous_override_reading (capitulation under duress, doctrine unchanged) or the hybrid_pragmatic_reading (strategic adaptation through scope ambiguity), adopted in place of this one, change the constraint''s structure?',
    'The readings are resolved by the causal-authority adjudication tracked in the causal_attribution omega; no single framework holds contradictory attributions, so the readings persist as separate constraints linked by the network, and corpus comparison — never in-story hedging — carries the contest.',
    'Adopting the exogenous sibling raises epsilon sharply and relocates the victim set to the whole membership; adopting the hybrid sibling splits the mechanism and softens the enforcement story. This story''s classification is valid only within its own attribution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Kernel contest: three readings of the Manifesto''s legitimacy with different epsilon and victim sets.').

omega_variable(
    causal_attribution_endogenous_vs_exogenous,
    'Was the reversal''s efficient cause the reported divine command, with federal pressure as mere catalyst — or was coercion the cause, with the revelation account constructed after the fact?',
    'Woodruff''s private journals and the documentation timeline of the reported revelation relative to the enforcement crescendo of 1889-1890; counselor correspondence; and Woodruff''s private versus public characterization of the Manifesto''s own status (''advice'' versus ''command'').',
    'If coercion is the efficient cause, this reading''s epsilon rises sharply and the constraint migrates toward the exogenous sibling''s structure; the catalyst framing is the load-bearing wall of the low extraction figure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_attribution_endogenous_vs_exogenous, empirical, 'The reading''s load-bearing causal claim: revelation-caused versus coercion-caused reversal.').

omega_variable(
    revelation_verifiability,
    'Can the reported revelation be verified at all, or is the claim structurally insulated from external check by the prophetic epistemology that constitutes the reading?',
    'Internal documentary evidence only: whether a contemporaneous revelation record exists and whether the account coheres with Woodruff''s documented decision sequence; external verification is structurally unavailable without adopting the epistemology being tested.',
    'If no contemporaneous revelation record exists and the account is retrospective, the legitimacy claim rests on institutional assertion alone and the reading collapses toward the hybrid or exogenous siblings; if the record is solid, the low extraction figure stands on its own warrant.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(revelation_verifiability, conceptual, 'Verifiability of the divine-command claim that constitutes the reading.').

omega_variable(
    sacrifice_vs_extraction_costs,
    'Are the costs borne by plural families and dissenters covenant sacrifice freely accepted under the reading''s lights, or costs borne under identity commitments that foreclosed refusal?',
    'Exit-cost reconstruction: whether refusal was materially available to a plural wife in 1890-1910 or to a dissenting elder, given that leaving cost family, property, community, and salvation-as-understood simultaneously; and how post-severance fundamentalists themselves describe the choice.',
    'If the costs are sacrifice, the constraint sits near pure coordination at the membership seats and the low epsilon stands; if extraction, the payer seats compute toward the snare end and the reading''s own lights understate effective extraction for exactly the seats that bore the costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sacrifice_vs_extraction_costs, preference, 'Whether identity-locked consent converts concentrated costs into sacrifice or extraction.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (discipline, prosecution, severance) or internalized (covenant identity making doubt self-suppressing), and in what proportion?',
    'Post-severance trajectory: fundamentalists expelled for rejecting the Manifesto''s authority typically retained their faith while rejecting the institutional reading — suggesting practice-suppression was structural while doubt-suppression among stayers was internalized; testimony patterns and private diaries across the interval distinguish the two.',
    'If substantially internalized, effective suppression exceeds the structural measure — enforcement capacity could decay (as the 1918-1930 series shows) without the boundary weakening, and the falling suppression tail understates the constraint''s hold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression in a covenant-identity community.').

omega_variable(
    theological_continuity_reframing,
    'Does the reframing — monogamy as the new covenant stage while the earlier marriage revelation stays canon — genuinely preserve theological continuity, or constitute tacit doctrinal revision maintained by scope ambiguity?',
    'Doctrinal analysis of official teaching across the interval: how the coexistence of the 1843 marriage revelation with the 1890 suspension was explained; whether an authoritative reconciliation was ever issued or the ambiguity was left to do the work.',
    'If revision, the continuity claim is performative, the theater series understates the maintenance burden, and the arrangement drifts toward inertial upkeep of a suspended doctrine; if continuity, the reframing is the coordination achievement this reading claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_continuity_reframing, conceptual, 'Continuity versus tacit revision in the monogamy-as-new-stage reframing.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the kernel the Manifesto text itself (a fixed, deliberately ambiguous text whose interpretation absorbed the drift), or the prophetic-succession legitimacy claim the text is deployed to maintain?',
    'Compare classifications under both framings: text-as-kernel yields a fixed_text/lineage commitment system with drift migrating into interpretation; legitimacy-claim-as-kernel makes the authority structure self-referential and shifts authority_grounding toward extraction, since the office benefits from preventing kernel revision.',
    'Under the legitimacy-claim framing, this reading''s maintenance looks more self-serving and the officeholder seat''s extraction rises; under the text framing, the ambiguity is the mechanism and the low epsilon is more defensible. The declared framing (fixed_text/lineage) follows the text; the alternative is live.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'CS-framing under-determination: text-kernel versus legitimacy-claim-kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 1890, 1930).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1890, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1890, 0.2).
narrative_ontology:measurement_basis(marr_tr_t1890, observed).
narrative_ontology:measurement(marr_tr_t1896, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1896, 0.22).
narrative_ontology:measurement_basis(marr_tr_t1896, observed).
narrative_ontology:measurement(marr_tr_t1904, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1904, 0.28).
narrative_ontology:measurement_basis(marr_tr_t1904, observed).
narrative_ontology:measurement(marr_tr_t1907, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1907, 0.32).
narrative_ontology:measurement_basis(marr_tr_t1907, observed).
narrative_ontology:measurement(marr_tr_t1911, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1911, 0.35).
narrative_ontology:measurement_basis(marr_tr_t1911, observed).
narrative_ontology:measurement(marr_tr_t1918, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1918, 0.38).
narrative_ontology:measurement_basis(marr_tr_t1918, observed).
narrative_ontology:measurement(marr_tr_t1925, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1925, 0.42).
narrative_ontology:measurement_basis(marr_tr_t1925, observed).
narrative_ontology:measurement(marr_tr_t1930, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1930, 0.45).
narrative_ontology:measurement_basis(marr_tr_t1930, observed).

% Extraction over time
narrative_ontology:measurement(marr_be_t1890, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1890, 0.3).
narrative_ontology:measurement_basis(marr_be_t1890, observed).
narrative_ontology:measurement(marr_be_t1896, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1896, 0.32).
narrative_ontology:measurement_basis(marr_be_t1896, observed).
narrative_ontology:measurement(marr_be_t1904, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1904, 0.38).
narrative_ontology:measurement_basis(marr_be_t1904, observed).
narrative_ontology:measurement(marr_be_t1907, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1907, 0.4).
narrative_ontology:measurement_basis(marr_be_t1907, observed).
narrative_ontology:measurement(marr_be_t1911, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1911, 0.42).
narrative_ontology:measurement_basis(marr_be_t1911, observed).
narrative_ontology:measurement(marr_be_t1918, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1918, 0.4).
narrative_ontology:measurement_basis(marr_be_t1918, observed).
narrative_ontology:measurement(marr_be_t1925, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1925, 0.37).
narrative_ontology:measurement_basis(marr_be_t1925, observed).
narrative_ontology:measurement(marr_be_t1930, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1930, 0.35).
narrative_ontology:measurement_basis(marr_be_t1930, observed).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1890, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 1890, 0.35).
narrative_ontology:measurement_basis(marr_su_t1890, observed).
narrative_ontology:measurement(marr_su_t1896, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 1896, 0.45).
narrative_ontology:measurement_basis(marr_su_t1896, observed).
narrative_ontology:measurement(marr_su_t1904, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 1904, 0.65).
narrative_ontology:measurement_basis(marr_su_t1904, observed).
narrative_ontology:measurement(marr_su_t1907, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 1907, 0.7).
narrative_ontology:measurement_basis(marr_su_t1907, observed).
narrative_ontology:measurement(marr_su_t1911, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 1911, 0.75).
narrative_ontology:measurement_basis(marr_su_t1911, observed).
narrative_ontology:measurement(marr_su_t1918, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 1918, 0.72).
narrative_ontology:measurement_basis(marr_su_t1918, observed).
narrative_ontology:measurement(marr_su_t1925, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 1925, 0.65).
narrative_ontology:measurement_basis(marr_su_t1925, observed).
narrative_ontology:measurement(marr_su_t1930, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 1930, 0.6).
narrative_ontology:measurement_basis(marr_su_t1930, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, marriage_commitment_legitimacy__exogenous_override_reading).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, marriage_commitment_legitimacy__hybrid_pragmatic_reading).

% DUAL FORMULATION NOTE:
% The kernel marriage_commitment_legitimacy decomposes into three readings because the causal-authority attribution of the 1890 Manifesto changes epsilon, the victim set, and the type: this endogenous reading holds extraction low (costs are covenant sacrifice; divine authority is the beneficiary); the exogenous_override_reading relocates the same costs as extraction from a coerced membership (high epsilon); the hybrid_pragmatic_reading splits the mechanism (strategic management preserving doctrine through scope ambiguity). Each story carries its own epsilon over the same standing arrangement — the Church's post-1890 authority-and-covenant structure — per the epsilon-invariance principle; the readings are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
