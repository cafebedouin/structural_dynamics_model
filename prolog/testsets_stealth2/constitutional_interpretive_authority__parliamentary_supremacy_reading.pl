% ============================================================================
% CONSTRAINT STORY: constitutional_interpretive_authority__parliamentary_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_interpretive_authority__parliamentary_supremacy_reading, []).

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
 *   constraint_id: constitutional_interpretive_authority__parliamentary_supremacy_reading
 *   human_readable: Parliamentary Supremacy Reading of Constitutional Interpretive Authority
 *   domain: constitutional law/political theory/jurisprudence
 *
 * SUMMARY:
 *   The arrangement under authorship is the parliamentary-supremacy
 *   allocation of constitutional interpretive authority: the elected
 *   legislature's reading of the constitution prevails, and no court may void
 *   an Act of Parliament. It solves a real coordination problem — finality
 *   and democratic authorization of ultimate constitutional voice — while
 *   concentrating unreviewable discretion in whoever holds the chamber, with
 *   the costs falling on those who can never assemble a majority. This story
 *   is ONE READING of the kernel constitutional_interpretive_authority
 *   (reading_id: parliamentary_supremacy_reading); the sibling readings
 *   judicial_supremacy_reading and coordinate_construction_reading
 *   instantiate different constraints from the same kernel and are separate
 *   stories linked via network.affects_constraints. The epsilon values differ
 *   across the family because each reading assesses the standing arrangement
 *   by its own lights: this reading authors epsilon approximately 0.35 for
 *   the parliamentary arrangement (costs are real but procedurally authorized
 *   by electoral mandate); the judicial supremacy reading authors
 *   substantially higher epsilon for the same arrangement (unreviewable
 *   statutes read as unprotected rights exposure); the coordinate
 *   construction reading authors moderate epsilon focused on
 *   finality-deadlock costs. The interval models 1905-2025 in year offsets (0
 *   = 1905, 120 = 2025).
 *
 * KEY AGENTS:
 *   - elected_legislature: agenda-setting institution ([institutional]/[arbitrage]) — administers the allocation and could rewrite it by ordinary majority
 *   - governing_parliamentary_majority: primary beneficiary ([powerful]/[mobile]) — collects the concentrated discretion each session
 *   - majoritarian_electorate: beneficiary with payer exposure ([organized]/[mobile]) — votes bind, but losers within it wait for the next election
 *   - statutory_minorities: primary target ([powerless]/[trapped]) — bear unreviewable statutes with no domestic forum
 *   - individual_rights_claimants: target ([moderate]/[constrained]) — remedies require legislative consent
 *   - national_judiciary: excluded actor ([institutional]/[trapped]) — applies but may not invalidate; would claim guardianship if admitted
 *   - opposition_parties: dual-positioned ([organized]/[mobile]) — pays while out of office, inherits the discretion when in
 *   - supranational_rights_bodies: excluded external reviewer ([institutional]/[constrained])
 *   - academic_constitutionalists: analytical observer — sees the full structure, decides nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.38).
domain_priors:suppression_score(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.48).
domain_priors:theater_ratio(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_interpretive_authority__parliamentary_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_interpretive_authority__parliamentary_supremacy_reading, "Parliamentary Supremacy Reading of Constitutional Interpretive Authority").
narrative_ontology:topic_domain(constitutional_interpretive_authority__parliamentary_supremacy_reading, "constitutional law/political theory/jurisprudence").

domain_priors:requires_active_enforcement(constitutional_interpretive_authority__parliamentary_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_interpretive_authority__parliamentary_supremacy_reading, '7bd3ed55-a1a9-47ca-8116-d1344b1a8819').
narrative_ontology:cs_kernel_codification('7bd3ed55-a1a9-47ca-8116-d1344b1a8819', formalized).
narrative_ontology:cs_authority_grounding('7bd3ed55-a1a9-47ca-8116-d1344b1a8819', lineage).
narrative_ontology:cs_interpretation_layer_present('7bd3ed55-a1a9-47ca-8116-d1344b1a8819').
narrative_ontology:cs_reading_relation('7bd3ed55-a1a9-47ca-8116-d1344b1a8819', constitutional_interpretive_authority__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('7bd3ed55-a1a9-47ca-8116-d1344b1a8819', constitutional_interpretive_authority__coordinate_construction_reading, forecloses).
narrative_ontology:cs_axiom('7bd3ed55-a1a9-47ca-8116-d1344b1a8819', foundational, electoral_mandate_confers_final_interpretive_authority).
narrative_ontology:cs_axiom_status(electoral_mandate_confers_final_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('7bd3ed55-a1a9-47ca-8116-d1344b1a8819', electoral_mandate_confers_final_interpretive_authority, deontological).
narrative_ontology:cs_axiom('7bd3ed55-a1a9-47ca-8116-d1344b1a8819', secondary, no_entrenched_substantive_limits_on_parliament).
narrative_ontology:cs_axiom_status(no_entrenched_substantive_limits_on_parliament, holdable).
narrative_ontology:cs_axiom_grounding('7bd3ed55-a1a9-47ca-8116-d1344b1a8819', no_entrenched_substantive_limits_on_parliament, conventional).
narrative_ontology:cs_axiom('7bd3ed55-a1a9-47ca-8116-d1344b1a8819', secondary, judicial_nullification_of_statutes_illegitimate).
narrative_ontology:cs_axiom_status(judicial_nullification_of_statutes_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('7bd3ed55-a1a9-47ca-8116-d1344b1a8819', judicial_nullification_of_statutes_illegitimate, conventional).
narrative_ontology:cs_reference_frame('7bd3ed55-a1a9-47ca-8116-d1344b1a8819', king_in_parliament_sovereignty_settlement).
narrative_ontology:cs_drift_state('7bd3ed55-a1a9-47ca-8116-d1344b1a8819', post_brexit_sovereignty_restoration, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('7bd3ed55-a1a9-47ca-8116-d1344b1a8819', '').
narrative_ontology:cs_kernel_id(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__parliamentary_supremacy_reading, elected_legislature).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__parliamentary_supremacy_reading, governing_parliamentary_majority).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__parliamentary_supremacy_reading, majoritarian_electorate).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__parliamentary_supremacy_reading, statutory_minorities).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__parliamentary_supremacy_reading, individual_rights_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__parliamentary_supremacy_reading, opposition_parties).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__parliamentary_supremacy_reading, majoritarian_electorate).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__parliamentary_supremacy_reading, opposition_parties).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__parliamentary_supremacy_reading, parliamentary_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__parliamentary_supremacy_reading, implied_repeal_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts every statute and settles what the constitution permits through the ordinary legislative process; no external body may set aside its products. It can reshape the settlement itself — entrench limits, create review courts, or abolish them — by ordinary majority, though it rarely does. Its members answer to voters, not to any constitutional tribunal.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, elected_legislature, agenda_setter,
    institutional, generational, arbitrage, national).

% Holds the concentrated discretion for the life of one parliament: its program becomes law immune from judicial invalidation, and its preferred reading of ambiguous constitutional questions prevails. When it loses an election the same discretion passes intact to its opponents.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, governing_parliamentary_majority, beneficiary,
    powerful, biographical, mobile, national).

% Sees its electoral choices translated directly into binding law without an intervening veto; the settlement is the mechanism by which its votes carry constitutional weight. Members of the majority who find themselves on the losing side of a particular statute bear the same exposure as any minority until the next election.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, majoritarian_electorate, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(constitutional_interpretive_authority__parliamentary_supremacy_reading, majoritarian_electorate, payer).

% Groups whose interests or identities never command a legislative majority live entirely under statutes they cannot challenge in any domestic court; their protection depends on persuading a future majority or, where treaties allow, bodies outside the national legal order. Leaving the jurisdiction is rarely feasible.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, statutory_minorities, payer,
    powerless, generational, trapped, national).

% Persons seeking remedies against the substance of an Act find courts willing to interpret generously but powerless to strike down; relief arrives only if the legislature consents to amend. Some obtain hearings before international bodies years later; most absorb the loss.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, individual_rights_claimants, payer,
    moderate, biographical, constrained, national).

% Applies statutes and settles meaning questions short of validity; senior judges publicly defend the declaratory theory while occasionally testing its edges in high-profile cases. They hold developed views on what a guardianship-of-fundamental-law role would look like but have no institutional path to claim it.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, national_judiciary, excluded,
    institutional, generational, trapped, national).

% Compete for the same concentrated discretion; today's opposition drafts tomorrow's majority's statutes. While out of office they bear the costs of statutes they opposed with no forum to contest them, and they campaign on promises to wield the very discretion they criticize.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, opposition_parties, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(constitutional_interpretive_authority__parliamentary_supremacy_reading, opposition_parties, payer).

% Treaty-based courts and committees outside the national legal order hear complaints the domestic system cannot resolve and publish findings the legislature may accept, ignore, or legislate past. They hold no seat in the domestic allocation of interpretive authority.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, supranational_rights_bodies, excluded,
    institutional, generational, constrained, continental).

% Document the doctrine's history, stress-test its boundary cases, and propose alternative allocations of final authority; their writings supply both the defense and the critique but decide nothing.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, academic_constitutionalists, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_interpretive_authority__parliamentary_supremacy_reading, governing_parliamentary_majority).
narrative_ontology:fixing_cost_class(constitutional_interpretive_authority__parliamentary_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves who decides what the constitution means and provides finality: disputes over constitutional meaning terminate in an authoritative legislative determination, avoiding inter-branch deadlock and giving electoral majorities a single channel for effectuating their mandate.
% TRANSFER_FUNCTION: Moves interpretive authority — and the immunity it confers — from courts and rights-holders to the sitting legislative majority; every statute carries a transfer of protection away from those who would have invoked judicial review toward those who benefit from its absence.
% ABSENT_VOICES: The national judiciary would claim a guardianship role if admitted; statutory minorities and individual claimants would ask for a forum that survives electoral reversal; future generations are bound by formally repealable acts they had no part in; supranational bodies object from outside the domestic allocation.
% DISAPPEARANCE_RATIONALE: If the allocation vanished overnight, courts would face an authority vacuum: either they assume nullification power (producing the judicial-supremacy arrangement) or a negotiated inter-branch regime emerges; ministerial reliance on unreviewable statutes, rights litigation strategy, and the legislative process itself would all reorganize around whichever successor allocation took hold.
% FOUNDING_PROBLEM: Settling where ultimate sovereign authority resides after the 1688-89 settlement: ending appeals to prerogative, divine right, or natural law above statute, and later forestalling inter-branch deadlock by locating final constitutional voice in the elected representatives of the people.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians corroborate the founding settlement and its anti-prerogative purpose from the documentary record of 1688-89 and the treatise tradition; corroboration that the ORIGINAL problem is dead comes from outside the benefiting parties — senior judges' extra-judicial writings, bar association reports, minority-rights organizations, and supranational treaty-body findings all attest that the arrangement now functions to shield sitting majorities rather than to settle crown-versus-parliament, while government briefs and parliamentary sovereignty-affirmation motions attest the problem is live in transformed form.
narrative_ontology:disappearance_verdict(constitutional_interpretive_authority__parliamentary_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_interpretive_authority__parliamentary_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_interpretive_authority__parliamentary_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_interpretive_authority__parliamentary_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.38, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_interpretive_authority__parliamentary_supremacy_reading_tests).
:- end_tests(constitutional_interpretive_authority__parliamentary_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are independent authored facts. I claim tangled_rope because the arrangement possesses BOTH a genuine coordination function (finality, democratic authorization, avoidance of inter-branch deadlock) AND asymmetric extraction (unreviewable discretion concentrated in the majority; costs borne by permanent minorities), and it requires active enforcement (the exclusion of judicial nullification must be continuously maintained against judicial probing, treaty-body pressure, and academic attack — it is not self-executing the way a physical regularity is). Extraction is authored at 0.38 from this reading's own lights: the referent is the standing parliamentary-supremacy arrangement itself, and within the reading's frame the costs on outvoted groups are procedurally authorized rather than illegitimate — hence well below what a judicial-supremacy reading would author for the identical arrangement. Suppression (0.48) is a raw structural property, NOT scaled by power or scope: the judicial alternative is structurally excluded, but the electoral channel stays open, so suppression is moderate. Theater (0.32) reflects the declaratory fictions and performative sovereignty affirmations that have grown as the doctrine required visible defense. Accessibility_collapse is low (0.35): rival allocations are visibly operating in peer democracies and domestically proposed, so alternatives do not vanish once the arrangement is understood. Resistance is moderate (0.50): sustained scholarly, judicial-probing, and treaty-channel resistance without frontal assault. The measurement series share one grid (every tracked metric at every time point 0-120 by twenties); the mid-series dip in base_extractiveness (t=80) records the EU-law primacy era, when courts could disapply statutes and the arrangement's core promise was externally qualified; the rising suppression_requirement series records the enforcement-capacity build-up needed to contain judicial probing (Factortame-era containment, Human Rights Act declaration management, post-Miller tension, express sovereignty clauses) — enforcement-capacity change is precisely the dynamic this story traces, which is why suppression_requirement is authored alongside the static structural suppression scalar (endpoint 0.55 vs scalar 0.48: containment effort runs ahead of achieved closure).
 *
 * PERSPECTIVAL GAP:
 *   The seats compute divergent types from identical structural data. From the governing majority's seat the arrangement is the coordination mechanism it operates through — finality plus mandate, near-rope experience. From the statutory-minority seat the same structure operates as enforced extraction with no exit — near-snare experience. The judiciary's excluded seat registers displacement rather than payment: it neither collects nor bears the fiscal transfer but is barred from the office the whole arrangement allocates. The engine computes these per-seat classifications from power, exit, and directional position; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for the legislature (agenda-setter with arbitrage-grade control of the rules), the sitting majority, and the electorate (mobile exit via the ballot). Victim declarations drive high directionality for statutory_minorities (trapped, no forum) and individual_rights_claimants (constrained, remedy-by-consent). The judiciary and supranational bodies are declared excluded rather than beneficiary or victim: they are outside the allocation the arrangement distributes, so no directionality override is needed — the derivation chain handles the seated parties, and the excluded seats are commentary-grade absences (they feed the absent-voices analysis, not classification overrides). No directionality_overrides are authored because beneficiary/victim plus exit data already yields the correct positions for every seated agent.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — settling sovereign authority after 1688-89 and preventing inter-branch deadlock — is genuinely contested: the anti-prerogative purpose is historically spent, but the deadlock-avoidance and democratic-authorization purposes remain argued as live by the benefiting parties and disputed as cover by critics outside them. Authoring status as contested (rather than dead) avoids manufacturing a zombie flag the record does not support; the mismatch consumer reads status x disappearance_verdict, and contested-plus-world_rearranges correctly flags neither capture nor obsolescence. Mandatrophy resolution here prevents two mislabels: reading the arrangement as pure extraction (snare) would erase the real coordination service — finality and mandate-channeling that every functioning democracy must supply somewhere; reading it as pure coordination (rope) would erase the uncompensated exposure of groups that can never win the electoral protection racket substitute. The tangled_rope claim keeps both halves on the table, and the receipt surface locates the asymmetry: gains accrue to the sitting majority (named seat), while fixing — introducing judicial review or entrenchment — is prohibitively costly for the only agent positioned to attempt it, because the legislature would have to disarm itself through the same ordinary majority the arrangement lets any future majority reverse.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is one reading of the kernel constitutional_interpretive_authority (reading: parliamentary_supremacy_reading); what structural changes would adoption of either sibling reading produce?',
    'Comparative institutional analysis of jurisdictions operating the sibling readings, tracking beneficiary-set composition, victim exposure, and enforcement overhead across adoption events (new constitutions, rights-instrument incorporation, court-curbing episodes).',
    'Under judicial_supremacy_reading the legislature exits the beneficiary set, statutory minorities gain a judicial protector, and the epsilon referent shifts to court-vetoable legislation; under coordinate_construction_reading no seat holds final authority and the transfer diffuses into inter-branch bargaining — both adoptions dissolve this story''s beneficiary/victim structure and require fresh stories rather than parameter edits.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: sibling-reading deltas for the interpretive-authority kernel.').

omega_variable(
    disagreement_location_exclusivity,
    'Where exactly do the three readings disagree — is final interpretive authority an exclusive office, and does that determine which sibling relations are foreclosures versus influences?',
    'Doctrinal analysis isolating the contested element: this reading and judicial_supremacy_reading treat finality as an exclusive office assigned to different branches (logically incompatible within one framework), while coordinate_construction_reading denies the office exists; test whether any hybrid framework can consistently hold two of the three core premises.',
    'If finality is genuinely exclusive, both foreclosure edges hold and the readings cannot blend; if a persuasive non-exclusive account succeeds, the foreclosure edges degrade toward influence relations and mixed regimes become stable attractors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disagreement_location_exclusivity, conceptual, 'Location of the kernel contest: exclusivity of the final-interpretive-authority office.').

omega_variable(
    entrenchment_possibility,
    'Can a parliamentary supremacy system entrench substantive limits against a future simple majority, or is every protection formally repealable by the same process that created it?',
    'Manner-and-form analysis and comparative study of attempted entrenchments (referendum locks, super-majority requirements, entrenchment clauses) and how courts have treated them.',
    'If entrenchment is impossible, minority exposure under this arrangement is total and the coordination framing weakens toward extraction; if manner-and-form entrenchment works, a protective transitional layer exists inside the arrangement and part of the measured extraction is contingent rather than structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(entrenchment_possibility, empirical, 'Whether unreviewable legislative discretion is escapable from within the system.').

omega_variable(
    electoral_mandate_sufficiency_for_minorities,
    'Does periodic election adequately protect groups whose support never approaches a majority, or does the arrangement expose them to unchecked majority preference with no compensating mechanism?',
    'Longitudinal comparison of discrete-minority outcomes across parliamentary-supremacy and judicial-review jurisdictions, controlling for wealth and culture; coalition-formation studies of whether dispersed minorities can convert judicial-exclusion grievances into electoral leverage.',
    'If elections suffice, the measured extraction is democratically authorized cost and the rope component dominates; if not, the extraction is uncompensated and the classification shifts toward snare at the affected seats — coalition potential among powerless victims is the swing variable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(electoral_mandate_sufficiency_for_minorities, empirical, 'Adequacy of the electoral channel as the sole protection mechanism.').

omega_variable(
    eu_interlude_identity,
    'Did the EU-law primacy era suspend this constraint in favor of a different one, or operate as a qualified continuation of it?',
    'Analysis of whether courts'' disapplication powers in the Factortame line constituted judicial review of statutes or enforcement of a hierarchically superior norm that Parliament itself had enacted and could withdraw.',
    'If suspension, the temporal series should be read as two constraints with a gap and the t=80 dip is a regime break; if continuation, the dip reflects internal qualification of one persistent constraint and the post-Brexit restoration is return rather than replacement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(eu_interlude_identity, conceptual, 'Identity of the mid-interval EU-law qualification of the arrangement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(cons_tr_t0, observed).
narrative_ontology:measurement(cons_tr_t20, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement_basis(cons_tr_t20, observed).
narrative_ontology:measurement(cons_tr_t40, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement_basis(cons_tr_t40, observed).
narrative_ontology:measurement(cons_tr_t60, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 60, 0.22).
narrative_ontology:measurement_basis(cons_tr_t60, observed).
narrative_ontology:measurement(cons_tr_t80, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 80, 0.26).
narrative_ontology:measurement_basis(cons_tr_t80, observed).
narrative_ontology:measurement(cons_tr_t100, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 100, 0.29).
narrative_ontology:measurement_basis(cons_tr_t100, observed).
narrative_ontology:measurement(cons_tr_t120, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 120, 0.32).
narrative_ontology:measurement_basis(cons_tr_t120, observed).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(cons_be_t0, observed).
narrative_ontology:measurement(cons_be_t20, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 20, 0.31).
narrative_ontology:measurement_basis(cons_be_t20, observed).
narrative_ontology:measurement(cons_be_t40, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 40, 0.34).
narrative_ontology:measurement_basis(cons_be_t40, observed).
narrative_ontology:measurement(cons_be_t60, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 60, 0.35).
narrative_ontology:measurement_basis(cons_be_t60, observed).
narrative_ontology:measurement(cons_be_t80, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 80, 0.33).
narrative_ontology:measurement_basis(cons_be_t80, observed).
narrative_ontology:measurement(cons_be_t100, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 100, 0.36).
narrative_ontology:measurement_basis(cons_be_t100, observed).
narrative_ontology:measurement(cons_be_t120, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 120, 0.38).
narrative_ontology:measurement_basis(cons_be_t120, observed).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(cons_su_t0, observed).
narrative_ontology:measurement(cons_su_t20, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 20, 0.28).
narrative_ontology:measurement_basis(cons_su_t20, observed).
narrative_ontology:measurement(cons_su_t40, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 40, 0.32).
narrative_ontology:measurement_basis(cons_su_t40, observed).
narrative_ontology:measurement(cons_su_t60, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 60, 0.38).
narrative_ontology:measurement_basis(cons_su_t60, observed).
narrative_ontology:measurement(cons_su_t80, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 80, 0.46).
narrative_ontology:measurement_basis(cons_su_t80, observed).
narrative_ontology:measurement(cons_su_t100, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 100, 0.51).
narrative_ontology:measurement_basis(cons_su_t100, observed).
narrative_ontology:measurement(cons_su_t120, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 120, 0.55).
narrative_ontology:measurement_basis(cons_su_t120, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_interpretive_authority__parliamentary_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__parliamentary_supremacy_reading, judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__parliamentary_supremacy_reading, coordinate_construction_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'who interprets the constitution' decomposes into three structurally distinct constraints sharing one kernel (constitutional_interpretive_authority). This story (parliamentary_supremacy_reading) authors epsilon approximately 0.35 for the parliamentary arrangement from its own lights; judicial_supremacy_reading authors substantially higher epsilon for the same arrangement (unreviewable statutes as unprotected rights exposure) and lower epsilon for court-vetoable legislation; coordinate_construction_reading authors moderate epsilon centered on deadlock and diffusion costs. The upstream member by empirical confidence is the parliamentary reading (longest continuous operation, richest doctrinal record); it influences the siblings because its historical settlement supplies the baseline against which both rivals define themselves. All three files cross-link via network.affects_constraints per the epsilon-invariance decomposition rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
