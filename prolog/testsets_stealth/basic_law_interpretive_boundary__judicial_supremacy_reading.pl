% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_boundary__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_boundary__judicial_supremacy_reading, []).

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
 *   constraint_id: basic_law_interpretive_boundary__judicial_supremacy_reading
 *   human_readable: Basic Laws as Binding Higher Law — Judicial Supremacy Reading
 *   domain: constitutional/legal-political
 *
 * SUMMARY:
 *   Under the judicial supremacy reading, Israel's Basic Laws constitute a
 *   higher-order legal framework: the Supreme Court interprets them as
 *   binding norms, ordinary Knesset legislation conflicting with them is
 *   void, and the invalidation binds the legislature. The reading entered
 *   positive practice with the 1992 Basic Law: Human Dignity and Liberty and
 *   the Court's 1995 United Mizrahi Bank decision, and it converts every
 *   contested policy field — conscription, settlement, religion-state
 *   balances, administrative reasonableness — into potential constitutional
 *   litigation. This file instantiates ONE reading of the kernel
 *   basic_law_interpretive_boundary; the parliamentary sovereignty and
 *   balanced contestation readings are separate constraints in separate files
 *   with their own epsilon values and victim sets, linked through the network
 *   block. The claim/metric gap is deliberate: claimed_type is authored from
 *   the analytical seat as tangled_rope (genuine commitment-device
 *   coordination plus asymmetric transfer of final authority, actively
 *   enforced), while the metrics describe the arrangement's actual operation
 *   — the engine measures the divergence rather than the author reconciling
 *   it. The expected structural delta is realized: the Court enters as
 *   enforcer, Knesset legislation is subject to nullification,
 *   rights-claimants hold a litigation veto, and the arrangement bears
 *   heavily on legislation threatening court-protected liberties.
 *
 * KEY AGENTS:
 *   - supreme_court_justices: agenda_setter and primary beneficiary (institutional / identity_locked) — administers the interpretive boundary, collects final-decision authority over legislation
 *   - individual_rights_claimants: beneficiary (moderate / constrained) — hold a litigation veto over rights-infringing statutes, exercised through NGOs and Court access
 *   - knesset_minority_factions: secondary beneficiary (moderate / constrained) — legislative positions sheltered from coalition override via petitions
 *   - knesset_majority_coalitions: primary payer (powerful / constrained) — enactments voidable after passage; formal amendment power hedged in practice
 *   - majoritarian_policy_constituencies: payer (powerless / trapped) — electoral victories judicially reversible after the fact
 *   - non_litigating_public: excluded (powerless / trapped) — absorbs policy volatility and stalled legislation without standing or voice
 *   - comparative_constitutional_scholars: observer (analytical / analytical) — maps the arrangement against other review regimes; decides nothing, collects nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.62).
domain_priors:suppression_score(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.58).
domain_priors:theater_ratio(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_boundary__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_boundary__judicial_supremacy_reading, "Basic Laws as Binding Higher Law — Judicial Supremacy Reading").
narrative_ontology:topic_domain(basic_law_interpretive_boundary__judicial_supremacy_reading, "constitutional/legal-political").

domain_priors:requires_active_enforcement(basic_law_interpretive_boundary__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_boundary__judicial_supremacy_reading, 'ef9ad6b7-0d9b-4650-8996-143f7ba2a682').
narrative_ontology:cs_kernel_codification('ef9ad6b7-0d9b-4650-8996-143f7ba2a682', fixed_text).
narrative_ontology:cs_authority_grounding('ef9ad6b7-0d9b-4650-8996-143f7ba2a682', lineage).
narrative_ontology:cs_interpretation_layer_present('ef9ad6b7-0d9b-4650-8996-143f7ba2a682').
narrative_ontology:cs_reading_relation('ef9ad6b7-0d9b-4650-8996-143f7ba2a682', basic_law_interpretive_boundary__parliamentary_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('ef9ad6b7-0d9b-4650-8996-143f7ba2a682', basic_law_interpretive_boundary__balanced_contestation_reading, forecloses).
narrative_ontology:cs_axiom('ef9ad6b7-0d9b-4650-8996-143f7ba2a682', foundational, basic_laws_constitute_superior_positive_law).
narrative_ontology:cs_axiom_status(basic_laws_constitute_superior_positive_law, holdable).
narrative_ontology:cs_axiom_grounding('ef9ad6b7-0d9b-4650-8996-143f7ba2a682', basic_laws_constitute_superior_positive_law, conventional).
narrative_ontology:cs_axiom('ef9ad6b7-0d9b-4650-8996-143f7ba2a682', foundational, rights_protection_requires_judicial_enforcement_against_majorities).
narrative_ontology:cs_axiom_status(rights_protection_requires_judicial_enforcement_against_majorities, holdable).
narrative_ontology:cs_axiom_grounding('ef9ad6b7-0d9b-4650-8996-143f7ba2a682', rights_protection_requires_judicial_enforcement_against_majorities, instrumental).
narrative_ontology:cs_reference_frame('ef9ad6b7-0d9b-4650-8996-143f7ba2a682', basic_laws_as_binding_supreme_law).
narrative_ontology:cs_drift_state('ef9ad6b7-0d9b-4650-8996-143f7ba2a682', post_2023_judicial_reform_crisis, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('ef9ad6b7-0d9b-4650-8996-143f7ba2a682', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_boundary__judicial_supremacy_reading, basic_law_interpretive_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__judicial_supremacy_reading, supreme_court_justices).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__judicial_supremacy_reading, individual_rights_claimants).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__judicial_supremacy_reading, knesset_minority_factions).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__judicial_supremacy_reading, knesset_majority_coalitions).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__judicial_supremacy_reading, majoritarian_policy_constituencies).
narrative_ontology:constraint_vindicates(basic_law_interpretive_boundary__judicial_supremacy_reading, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(basic_law_interpretive_boundary__judicial_supremacy_reading, counter_majoritarian_guardianship_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A fifteen-member bench sitting both as final appellate court and as High Court of Justice. Since the 1995 United Mizrahi Bank decision it treats the Basic Laws as superior norms and strikes ordinary statutes that conflict with them. Its docket, its leverage over judicial selection, and its public standing all depend on continuing to perform this role; a justice who renounced the review power would dissolve the office's own foundation. Leaving the bench means retirement, not repositioning.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, supreme_court_justices, agenda_setter,
    institutional, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__judicial_supremacy_reading, supreme_court_justices, beneficiary).

% Petitioners who challenge statutes and state action burdening speech, equality, religion, property, or bodily autonomy. Their practical veto runs through civil-rights organizations and through the Court's willingness to hear petitions; without access to the bench their position reverts to whatever the governing coalition enacts. Their leverage is real but wholly mediated by the institution they petition.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, individual_rights_claimants, beneficiary,
    moderate, biographical, constrained, national).

% Opposition and small-coalition parties whose bills routinely fail in plenary votes but frequently succeed as petitions. Judicial protection substitutes for their numerical weakness; their voters' interests reach policy through the courtroom rather than through majorities. Their stake survives only as long as the bench keeps hearing such petitions.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, knesset_minority_factions, beneficiary,
    moderate, biographical, constrained, national).

% The governing bloc that commands the plenary and drafts ordinary legislation. Its products are voidable after enactment by a bench it did not choose and cannot readily reshape; its formal power to amend Basic Laws by majority is hedged in practice by the prospect that amendments themselves will be reviewed. Attempts to clip the review power — override clauses, curbing the reasonableness ground — have triggered mass protest, repeated elections, and institutional standoffs over judicial appointments.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, knesset_majority_coalitions, payer,
    powerful, immediate, constrained, national).

% Voters whose preferred policies — conscription arrangements, settlement policy, religion-state balances — have been blocked or reversed by court decisions after winning elections. Their remedy is the ballot, but the ballot produces legislation the bench can undo, so their leverage over final outcomes is systematically weaker than their electoral weight suggests.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, majoritarian_policy_constituencies, payer,
    powerless, biographical, trapped, national).

% Citizens who neither petition nor organize: they absorb policy volatility, prolonged legal uncertainty over major public arrangements, and the opportunity costs of legislation stalled in years-long litigation, without standing or voice in the process that decides these questions.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, non_litigating_public, excluded,
    powerless, biographical, trapped, national).

% Academics and comparative constitutionalists in Israel and abroad who map the arrangement against other polities' review regimes, testify to committees, and supply the conceptual vocabulary both camps deploy. They decide nothing and collect nothing.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, comparative_constitutional_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_boundary__judicial_supremacy_reading, supreme_court_justices).
narrative_ontology:fixing_cost_class(basic_law_interpretive_boundary__judicial_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the constitutional commitment problem: a polity with no completed written constitution needs a mechanism that binds future legislative majorities to entrenched rights guarantees despite each majority's incentive to defect; a single authoritative interpreter supplies consistent resolution of statute-versus-Basic-Law conflicts that ad hoc political bargaining cannot.
% TRANSFER_FUNCTION: Moves final decision authority over the validity of ordinary legislation from the elected Knesset to the Supreme Court; moves a litigation-based veto to rights-claimants and their organizations; moves the cost of nullified legislative labor and blocked policy programs to governing coalitions and the constituents who elected them.
% ABSENT_VOICES: Non-litigating publics whose policy preferences are invalidated but who lack standing, resources, or organizational representation before the Court; the founding generation's constituent intent — no body ever ratified judicial supremacy, and the 1950 Harari Resolution's gradualist compact said nothing about review; and voters treated as rights-claimants only when they win in court. They are outside the room because access is gated by standing, litigation capacity, and judicial discretion.
% DISAPPEARANCE_RATIONALE: If judicial invalidation vanished overnight: pending constitutional petitions collapse, coalition legislative programs previously struck or frozen proceed, the Court's docket and institutional weight contract sharply, rights-claimants lose their litigation veto and revert to ordinary politics, and the entire separation-of-powers settlement rearranges around restored legislative finality.
% FOUNDING_PROBLEM: Israel's 1948 founding produced no completed constitution; the 1950 Harari Resolution delegated chapter-by-chapter enactment of 'Basic Laws' to a legislature that understood itself as the constituent assembly. The unresolved problem: how to limit a sovereign-feeling parliament and protect individual rights in a system with no entrenched supreme text and no agreed repository of constituent power.
% FOUNDING_PROBLEM_CORROBORATION: Corroborators outside the benefiting parties: the Knesset's own constitutional records and the Harari Resolution text, which granted no review power; Israeli constitutional historians across the interpretive spectrum, who attest both the founding vacuum and the contested character of the Court's 1995 self-adoption of review; and the 2023-25 legislative record — override-clause bills, the reasonableness-limitation amendment, appointment standoffs — in which a large elected bloc explicitly denies that judicial supremacy solves the founding problem. No corroborator outside the beneficiary set attests that the founding problem is fully dead; equally, none attests that the judicial-supremacy settlement was the ratified answer to it.
narrative_ontology:disappearance_verdict(basic_law_interpretive_boundary__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_boundary__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_boundary__judicial_supremacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(basic_law_interpretive_boundary__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_boundary__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_boundary__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_boundary__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is 0.62 at interval end: the arrangement transfers final decision authority over ordinary legislation from an elected body to an unelected bench, decoupled from any electoral mandate and imposed retroactively on enacted labor — but it rides a real commitment device, and the referent is the standing arrangement assessed by this reading's own lights, which regard the transfer as legitimate higher-law enforcement rather than predation; hence substantially-but-not-maximally extractive. Suppression is 0.58 and is authored as a raw structural property — it is NOT scaled by power or scope; only extractiveness is scaled by directionality and spatial scope in the engine's computation. Persistence requires active defense: review of override attempts, leverage over judicial selection, and doctrinal maintenance against a legislature openly trying to clip the power, yet the Knesset retains a formal amendment channel, so suppression sits mid-high rather than extreme. Theater is low (0.18): the review function is performed, not merely announced. Accessibility_collapse is 0.55: once the reading is accepted, override and simple-majority amendment routes partially collapse, but formal routes persist pending the amendment-review omega. Resistance is 0.72: the 2023-25 legislative assault (override-clause bills, the reasonableness-limitation amendment, selection-committee boycotts) is among the strongest recorded resistances to a judicial-review settlement anywhere. The temporal series run on one shared grid — all three metrics authored at all six points — so no end-state value is injected backward. The trajectories are monotonic ratchets, not oscillations: episodic crisis-and-resolution (2023 spike, partial de-escalation) sits atop a steady accumulation of extracted legislative authority and defensive enforcement effort, so no intermittent-reinforcement mechanism is claimed.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats compute differently from identical structural data. From the governing-coalition seat the arrangement operates as post-hoc destruction of enacted legislation by a bench it cannot readily reshape — snare-flavored experience. From the rights-claimant seat the same structure is the only working guarantee against transient majorities — rope-flavored experience. From the bench it is guardianship duty. On the coalition-power check: majoritarian_policy_constituencies are numerous enough to coordinate, but their only lever is the electoral channel, and the arrangement renders that channel's outputs reversible — so their latent coalition power is systematically blunted, which is what pins them near the trapped pole despite their numbers. The engine computes these per-seat divergences from power and exit data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: supreme_court_justices sit nearest the beneficiary pole (they collect the transferred authority and administer the rules that produce it); individual_rights_claimants and knesset_minority_factions derive low-to-moderate d (net subsidized, though their benefit is contingent on the arrangement's survival). Victim declarations drive high d: knesset_majority_coalitions bear the transfer directly with constrained exit (formal amendment power hedged by prospective review), and majoritarian_policy_constituencies sit nearest the full-target pole — trapped, because their remedy is the very channel the arrangement neutralizes. No directionality overrides are needed: the derivation from beneficiary/victim declarations plus exit options reproduces the structural relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a constitutional vacuum left by the unfinished Harari compact, with no agreed repository of constituent power — is contested rather than dead: the demand for rights-guardianship persists, but the specific settlement (self-adopted judicial supremacy) is disputed by a large legislative bloc. The tangled_rope classification guards against both mislabels: calling the arrangement a snare erases the genuine coordination function (solving the majority-cycle commitment problem no elected majority can solve alone); calling it a rope erases the asymmetric transfer (final authority moved from the elected Knesset to the Court without ratification by any constituent body). If the founding problem were ever resolved by a broadly ratified completed constitution, the residual enforcement machinery would drift piton-ward — maintained theatrically around a settled text — and the mismatch consumer would flag status-dead-plus-world-rearranges accordingly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constituted_vs_discovered_supremacy,
    'Is the Basic Laws'' higher-order status a pre-existing legal fact the Supreme Court enforces, or a status constituted by the Court''s own 1995 United Mizrahi Bank practice?',
    'Comparative analysis of entrenchment clauses across jurisdictions, plus the counterfactual record: whether any actor other than the Court treated the 1992 Basic Laws as supreme prior to judicial adoption of review.',
    'If the status is constituted rather than discovered, part of the measured transfer of authority is self-authorizing appropriation of constituent power, the lineage grounding weakens toward extraction-grounding, and epsilon rises; if discovered, the arrangement is genuine higher-law enforcement and epsilon reflects the price of constitutionalism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constituted_vs_discovered_supremacy, conceptual, 'Whether Basic Law supremacy is a discovered fact or a Court-constituted status.').

omega_variable(
    amendment_review_boundary,
    'Are Knesset amendments to the Basic Laws themselves subject to substantive judicial review (an unconstitutional-constitutional-amendment doctrine)?',
    'A future Court ruling on a directly challenged amendment — the petitions against the 2023 reasonableness-limitation amendment present exactly this question.',
    'If amendments are reviewable, the Knesset''s formal exit route closes, suppression and accessibility_collapse climb toward snare-range values, and the arrangement hardens; if not, exit remains real and the arrangement stays hybrid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_review_boundary, empirical, 'Whether the legislature''s formal amendment channel is substantively open or judicially hedged.').

omega_variable(
    persistence_source_legitimacy_vs_doctrine,
    'Does the arrangement persist because delivered rights-protection earns performance legitimacy, or through doctrinal self-reproduction insulated from electoral feedback?',
    'Panel-composition and docket studies crossed with public-opinion tracking across coalition and policy shocks; divergence between approval of specific rulings and approval of the review power itself.',
    'If persistence is doctrinal self-reproduction, the rising suppression_requirement series indicates a defensive ratchet with snare-drift risk; if performance legitimacy, the same series reflects stable adjudication demand and the hybrid classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(persistence_source_legitimacy_vs_doctrine, empirical, 'Source of the arrangement''s persistence: delivered benefits versus closed doctrinal loop.').

omega_variable(
    kernel_reading_indexicality,
    'This file instantiates one reading (judicial_supremacy_reading) of kernel basic_law_interpretive_boundary; the sibling readings (parliamentary_sovereignty_reading, balanced_contestation_reading) instantiate different constraints with different victim sets — how should cross-reading comparison be indexed?',
    'File-by-file comparison only: the disagreement among readings is located in a single structural element — the locus of final interpretive authority — so each reading''s epsilon, beneficiaries, and victims are indexical to that reading and must not be pooled.',
    'Under the parliamentary sovereignty sibling the victim set inverts (claimants stripped of review become the exposed class); pooling epsilon across readings would average away exactly the structural difference the kernel exists to measure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame routing: one reading, one constraint, one epsilon; siblings are separate files.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_boundary__judicial_supremacy_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bl_interp_jsr_tr_t0, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(bl_interp_jsr_tr_t0, observed).
narrative_ontology:measurement(bl_interp_jsr_tr_t6, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 6, 0.11).
narrative_ontology:measurement_basis(bl_interp_jsr_tr_t6, observed).
narrative_ontology:measurement(bl_interp_jsr_tr_t12, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 12, 0.13).
narrative_ontology:measurement_basis(bl_interp_jsr_tr_t12, observed).
narrative_ontology:measurement(bl_interp_jsr_tr_t18, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 18, 0.15).
narrative_ontology:measurement_basis(bl_interp_jsr_tr_t18, observed).
narrative_ontology:measurement(bl_interp_jsr_tr_t24, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 24, 0.17).
narrative_ontology:measurement_basis(bl_interp_jsr_tr_t24, observed).
narrative_ontology:measurement(bl_interp_jsr_tr_t30, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement_basis(bl_interp_jsr_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(bl_interp_jsr_be_t0, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(bl_interp_jsr_be_t0, observed).
narrative_ontology:measurement(bl_interp_jsr_be_t6, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 6, 0.48).
narrative_ontology:measurement_basis(bl_interp_jsr_be_t6, observed).
narrative_ontology:measurement(bl_interp_jsr_be_t12, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 12, 0.52).
narrative_ontology:measurement_basis(bl_interp_jsr_be_t12, observed).
narrative_ontology:measurement(bl_interp_jsr_be_t18, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 18, 0.56).
narrative_ontology:measurement_basis(bl_interp_jsr_be_t18, observed).
narrative_ontology:measurement(bl_interp_jsr_be_t24, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 24, 0.6).
narrative_ontology:measurement_basis(bl_interp_jsr_be_t24, observed).
narrative_ontology:measurement(bl_interp_jsr_be_t30, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement_basis(bl_interp_jsr_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(bl_interp_jsr_su_t0, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement_basis(bl_interp_jsr_su_t0, observed).
narrative_ontology:measurement(bl_interp_jsr_su_t6, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 6, 0.36).
narrative_ontology:measurement_basis(bl_interp_jsr_su_t6, observed).
narrative_ontology:measurement(bl_interp_jsr_su_t12, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 12, 0.42).
narrative_ontology:measurement_basis(bl_interp_jsr_su_t12, observed).
narrative_ontology:measurement(bl_interp_jsr_su_t18, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 18, 0.47).
narrative_ontology:measurement_basis(bl_interp_jsr_su_t18, observed).
narrative_ontology:measurement(bl_interp_jsr_su_t24, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 24, 0.53).
narrative_ontology:measurement_basis(bl_interp_jsr_su_t24, observed).
narrative_ontology:measurement(bl_interp_jsr_su_t30, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement_basis(bl_interp_jsr_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_boundary__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__judicial_supremacy_reading, basic_law_interpretive_boundary__parliamentary_sovereignty_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__judicial_supremacy_reading, basic_law_interpretive_boundary__balanced_contestation_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'the constitutional status of the Basic Laws' covers three structurally distinct claims, one per declared reading of kernel basic_law_interpretive_boundary, each with its own epsilon, beneficiary/victim sets, and classification. This file authors the judicial_supremacy_reading (epsilon 0.62; victims are legislative majorities and their constituents). The parliamentary_sovereignty_reading inverts the victim set — claimants stripped of review become the exposed class — and the balanced_contestation_reading distributes costs across both benches. The upstream member is the pre-1995 baseline practice; this reading is downstream of the 1992 Basic Law enactments and cites them as evidence of higher-law status. Cross-reading epsilon pooling is prohibited; compare classifications file-by-file only.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
