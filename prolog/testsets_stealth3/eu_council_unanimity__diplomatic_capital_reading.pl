% ============================================================================
% CONSTRAINT STORY: eu_council_unanimity__diplomatic_capital_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_council_unanimity__diplomatic_capital_reading, []).

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
 *   constraint_id: eu_council_unanimity__diplomatic_capital_reading
 *   human_readable: EU Council Unanimity Requirement — Diplomatic Capital Reading
 *   domain: political/institutional/international-relations
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the EU Council unanimity rule:
 *   unanimity as a consensus-building requirement whose iterative negotiation
 *   costs purchase legitimacy, buy-in, and durability. The referent of every
 *   metric is the standing unanimity arrangement itself, assessed by this
 *   reading's own lights — not the QMV alternative this reading would decline
 *   to endorse and not the sibling readings' characterizations. The
 *   colloquial label 'EU unanimity' decomposes, per the epsilon-invariance
 *   principle, into three structurally distinct claims held by different
 *   factions: this diplomatic-capital reading (low extraction, coordination
 *   cost with legitimacy payoff), a sovereignty-guarantor reading, and a
 *   veto-trap reading. Each is a separate constraint story with its own
 *   epsilon, beneficiaries, and classification; this file links to its
 *   siblings through network.affects_constraints and does not average over
 *   them. KEY AGENTS (by structural relationship): - large_member_states:
 *   principal underwriters (powerful/constrained) — supply most negotiating
 *   time and concessions - small_member_states: principal voice-collectors
 *   (organized/constrained) — equal formal say, outsized per-capita influence
 *   - rotating_council_presidency: procedural broker
 *   (institutional/constrained) — administers the process without deciding
 *   outcomes - european_commission: proposal owner turned legitimation
 *   beneficiary (institutional/constrained) -
 *   national_parliaments_and_publics: bound outsiders (moderate/constrained)
 *   — ratify packages they did not negotiate - third_country_aid_recipients:
 *   dependent outsiders (powerless/trapped) — await decisions they cannot
 *   shape - integration_scholars: analytical observer (analytical/analytical)
 *   — test the durability premium from outside
 *
 * KEY AGENTS:
 *   - large_member_states: principal underwriters (powerful/constrained) — supply most negotiating time and concessions
 *   - small_member_states: principal voice-collectors (organized/constrained) — equal formal say, outsized per-capita influence
 *   - rotating_council_presidency: procedural broker (institutional/constrained) — administers without deciding
 *   - european_commission: proposal owner turned legitimation beneficiary (institutional/constrained)
 *   - national_parliaments_and_publics: bound outsiders (moderate/constrained)
 *   - third_country_aid_recipients: dependent outsiders (powerless/trapped)
 *   - integration_scholars: analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_council_unanimity__diplomatic_capital_reading, 0.28).
domain_priors:suppression_score(eu_council_unanimity__diplomatic_capital_reading, 0.15).
domain_priors:theater_ratio(eu_council_unanimity__diplomatic_capital_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_council_unanimity__diplomatic_capital_reading, rope).
narrative_ontology:human_readable(eu_council_unanimity__diplomatic_capital_reading, "EU Council Unanimity Requirement — Diplomatic Capital Reading").
narrative_ontology:topic_domain(eu_council_unanimity__diplomatic_capital_reading, "political/institutional/international-relations").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eu_council_unanimity__diplomatic_capital_reading, '351f0634-5740-4af1-995b-c808ffeded5b').
narrative_ontology:cs_kernel_codification('351f0634-5740-4af1-995b-c808ffeded5b', formalized).
narrative_ontology:cs_authority_grounding('351f0634-5740-4af1-995b-c808ffeded5b', lineage).
narrative_ontology:cs_interpretation_layer_present('351f0634-5740-4af1-995b-c808ffeded5b').
narrative_ontology:cs_reading_relation('351f0634-5740-4af1-995b-c808ffeded5b', eu_council_unanimity__sovereignty_guarantor_reading, coexists_with).
narrative_ontology:cs_reading_relation('351f0634-5740-4af1-995b-c808ffeded5b', eu_council_unanimity__veto_trap_reading, coexists_with).
narrative_ontology:cs_axiom('351f0634-5740-4af1-995b-c808ffeded5b', foundational, consent_purchase_yields_durability).
narrative_ontology:cs_axiom_status(consent_purchase_yields_durability, holdable).
narrative_ontology:cs_axiom_grounding('351f0634-5740-4af1-995b-c808ffeded5b', consent_purchase_yields_durability, empirically_contingent).
narrative_ontology:cs_axiom('351f0634-5740-4af1-995b-c808ffeded5b', foundational, holdout_leverage_is_price_discovery).
narrative_ontology:cs_axiom_status(holdout_leverage_is_price_discovery, holdable).
narrative_ontology:cs_axiom_grounding('351f0634-5740-4af1-995b-c808ffeded5b', holdout_leverage_is_price_discovery, instrumental).
narrative_ontology:cs_reference_frame('351f0634-5740-4af1-995b-c808ffeded5b', consensus_legitimacy_framework).
narrative_ontology:cs_drift_state('351f0634-5740-4af1-995b-c808ffeded5b', contemporary_enlarged_union, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('351f0634-5740-4af1-995b-c808ffeded5b', '').
narrative_ontology:cs_kernel_id(eu_council_unanimity__diplomatic_capital_reading, eu_council_unanimity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_council_unanimity__diplomatic_capital_reading, large_member_states).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__diplomatic_capital_reading, small_member_states).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__diplomatic_capital_reading, european_commission).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(eu_council_unanimity__diplomatic_capital_reading, large_member_states).
narrative_ontology:constraint_victim(eu_council_unanimity__diplomatic_capital_reading, european_commission).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Governments such as Germany's and France's carry the largest share of the Union's agenda: they originate initiatives, bankroll package deals, and spend the most ministerial hours brokering texts every delegation can accept. What flows from them is negotiating time, concession space across unrelated dossiers, and the patience to shelve files a single partner refuses. What flows back is ownership of the resulting policy and the assurance that commitments they sign will be implemented everywhere. Leaving the arrangement would mean forfeiting access to the single market and the diplomatic weight of collective action; the United Kingdom's departure priced that exit.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, large_member_states, beneficiary,
    powerful, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(eu_council_unanimity__diplomatic_capital_reading, large_member_states, payer).

% Governments such as Malta's, Luxembourg's, and Ireland's hold the same formal say as the largest partners: nothing binds them without their yes. They contribute far fewer resources to the common enterprise but collect a voice in every dossier far beyond their size, and they can trade their assent for attention to their priority files. Exit would mean facing their larger neighbours one by one without the shield of equal standing.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, small_member_states, beneficiary,
    organized, generational, constrained, continental).

% The member state holding the six-month chair schedules meetings, drafts compromise texts, and brokers package deals, but cannot close any sensitive file without every delegation's consent. Its craft is finding formulations all can live with; its reward is agenda influence rather than any lasting claim on outcomes. Its position rotates on a treaty timetable, so it cannot opt out of the consent requirement for the files it inherits.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, rotating_council_presidency, agenda_setter,
    institutional, biographical, constrained, continental).

% The Commission drafts proposals and shepherds them through the Council. Sensitive files come back to it heavily amended and often delayed while governments trade concessions, yet the acts that emerge carry every government's signature, which translates into smoother implementation and fewer enforcement fights than decisions adopted over objections typically produce. It cannot withdraw the consent requirement from the files it manages.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, european_commission, beneficiary,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(eu_council_unanimity__diplomatic_capital_reading, european_commission, payer).

% Voters and their national assemblies are bound by bargains struck line-by-line in Brussels rooms they do not occupy. They can replace the government that said yes, but cannot reopen a closed package or split an omnibus trade they dislike; ratification reaches them, when it reaches them at all, as a single up-or-down choice.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, national_parliaments_and_publics, excluded,
    moderate, generational, constrained, national).

% Countries depending on jointly financed EU assistance — reconstruction support, sanction-related packages, accession funds — wait on decisions that require every member government's approval. A single government's objection can stall support they have no procedural means to influence; their recourse is persuasion through national capitals, not any seat in the room.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, third_country_aid_recipients, excluded,
    powerless, immediate, trapped, global).

% Political scientists and legal scholars track whether decisions adopted by full consent hold longer and implement better than decisions adopted over objections, and whether the price paid in delay and concession-buying is justified by that durability. They observe the full bargaining structure from outside and hold no stake in any particular file.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, integration_scholars, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eu_council_unanimity__diplomatic_capital_reading, diffuse).
narrative_ontology:fixing_cost_class(eu_council_unanimity__diplomatic_capital_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Forces collective decisions to be packaged and renegotiated until every member state consents, converting a twenty-seven-party preference-aggregation problem into a solvable bargaining problem. It guarantees that no state is bound by a policy it refused, which preserves voluntary participation in the collective enterprise and gives adopted acts a legitimacy that carries into implementation.
% TRANSFER_FUNCTION: Moves negotiating time, attention, and policy concessions from agenda-pushing states toward reluctant states, and moves consent and implementation fidelity back from all states to the collective. Package deals and side-payments redistribute policy value across sectors and across time; the legitimacy dividend is shared by every participant.
% ABSENT_VOICES: National parliaments and domestic publics are bound by package deals struck over their heads — they can punish governments electorally but cannot reopen closed bargains. Third-country aid recipients depend on unanimously approved measures they have no vote in. Candidate states live under accession conditions set unanimously. All three would object to specific trades if seated; their exclusion is what makes the intra-governmental unanimity achievable.
% DISAPPEARANCE_RATIONALE: If the unanimity requirement vanished overnight and qualified majority took its place in the remaining domains, stalled files would unblock immediately, small states would lose their guaranteed voice, the holdout-protection that keeps reluctant governments inside the bargain would disappear, and the intergovernmental equilibrium sustaining treaty compliance would reorganize around shifting coalitions and outvoted minorities. Every named seat's situation changes materially.
% FOUNDING_PROBLEM: How to enable sovereign states that had recently warred against each other to pool policy without a coercive centre — ensuring each government consents to what it must implement, after the empty-chair crisis demonstrated that majority decisions could be paralysed by outright refusal and that imposed decisions would not hold.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties by comparative political science on defection rates in majority-rule international bodies, by the historical record of the empty-chair crisis and its resolution, and by national constitutional courts' insistence on state consent as a condition of transferred competence. The EU institutions themselves are interested parties and their attestation of the founding problem is discounted accordingly.
narrative_ontology:disappearance_verdict(eu_council_unanimity__diplomatic_capital_reading, world_rearranges).
narrative_ontology:founding_problem_status(eu_council_unanimity__diplomatic_capital_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eu_council_unanimity__diplomatic_capital_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(eu_council_unanimity__diplomatic_capital_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eu_council_unanimity__diplomatic_capital_reading, 0.28, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_council_unanimity__diplomatic_capital_reading_tests).
:- end_tests(eu_council_unanimity__diplomatic_capital_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.28): the arrangement's costs are real — delay, concession inflation, holdout premiums — but under this reading those purchases produce implementable policy, and no party is systematically drained; costs scale with participation rather than concentrating on a fixed victim class. Suppression is low (0.15) and UNSCALED by design: the rule coerces no one, every state retains its veto, and alternatives (qualified majority in most domains, enhanced cooperation, opt-outs) remain open — the rule raises the price of acting without everyone rather than closing exits. Theater ratio is low-to-moderate (0.20): marathon summits and unity communiques carry performative content, but the consent-forging function is genuine work, not ritual. Accessibility collapse is moderate (0.45): alternatives demonstrably persist, since most Union legislation proceeds under qualified majority and governments have repeatedly chosen to keep unanimity only where they value it. Resistance is low-to-moderate (0.30): recurring campaigns to extend majority voting and occasional bypass instruments press against the rule, but member governments defend it where it shields them. The measurement series run on ONE shared six-point grid (t0 approximates the mid-1980s, when unanimity was the default decision mode; t40 the present enlarged union); no suppression_requirement series is authored because the enforcement picture is static — the rule is self-policed by the veto itself, with no enforcement machinery being built up or decaying.
 *
 * PERSPECTIVAL GAP:
 *   The engine computes per-seat classifications from the structural data, and the seats diverge sharply. From the small-state seat the rule is a subsidy: voice beyond size, guaranteed by treaty. From the large-state seat it is a tax on speed, paid in ministerial hours and concessions across unrelated dossiers. From the excluded seats — publics and third-country recipients — it is a lockout: decisions arrive closed. Same structure, three experiences; the divergence is computed from power and exit data, not asserted by the authoring claim.
 *
 * DIRECTIONALITY LOGIC:
 *   All three participant groups are declared beneficiaries, placing their derived directionality near the subsidized end. One override is declared: large_member_states sit at the 'powerful' atom, and a beneficiary-only derivation would place them near full beneficiary (~0.05-0.10), ignoring that they disproportionately SUPPLY the arrangement's principal inputs — negotiating time and concession space. The override sets d=0.30: still net beneficiary, but materially above the small-state seat, encoding the underwriter asymmetry. Excluded stakeholders are deliberately NOT routed into directionality: per the R3 ruling, authored absences are commentary-grade and must not drive classification; they inform absent_voices and the consensus-provenance check instead.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — voluntary cooperation among sovereigns without a coercive centre — remains live, so no mandatrophy resolution is declared and the mandate is intact. The classification discipline cuts both ways. Against over-reading extraction: this story documents a genuine coordination function, diffuse receipts, and open alternatives, which blocks a reflexive snare verdict on an arrangement whose costs are largely the price of its product. Against rope complacency: the omega battery keeps the two questions that could rot this reading permanently open — whether the durability premium is empirically real, and where legitimate price discovery ends and blocking rents begin. If the founding problem ever died (a union with coercive capacity of its own), the rule would persist as ceremony; the status-times-verdict mismatch consumer would flag that transition. Today the mismatch is absent: live status, world_rearranges verdict, functioning function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading (diplomatic_capital_reading) of the eu_council_unanimity kernel; sibling readings (sovereignty_guarantor_reading, veto_trap_reading) instantiate different constraints over the same treaty rule — what changes structurally if a sibling reading governs classification?',
    'Adjudicate by locating the disagreement — whether holdout leverage is legitimate price discovery (this reading), sovereignty protection (guarantor), or minoritarian extraction (trap) — using the empirical probes in the sibling omegas: durability-premium testing and concession-valence analysis.',
    'Under veto_trap_reading the same arrangement computes as a snare-flavored structure with high epsilon and identifiable victims (agenda-pushing states and affected outsider populations); under sovereignty_guarantor_reading it computes as a protective coordination device with a sovereignty-based beneficiary structure. This file''s low-epsilon verdict holds only within this reading''s frame.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer-frame contest over the unanimity kernel: reading-relative classification of the same treaty rule.').

omega_variable(
    durability_premium_empirics,
    'Do decisions adopted by full consent actually exhibit lower defection and higher implementation fidelity than qualified-majority decisions, net of selection effects (hard files may be routed to unanimity precisely because they need it)?',
    'Compare infringement and compliance rates for unanimous versus qualified-majority legislation, controlling for policy area, salience, and file difficulty; exploit variation from domains that migrated to qualified majority under passerelle provisions.',
    'If no durability premium survives controls, the legitimacy payoff collapses to assertion, the reading loses its warrant, and effective extraction rises toward pure coordination cost — drifting this constraint toward the veto_trap_reading''s structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(durability_premium_empirics, empirical, 'Whether the legitimacy payoff that anchors this reading is empirically real.').

omega_variable(
    holdout_valence_boundary,
    'Where is the boundary between legitimate price discovery (consent purchased at terms tracking the holdout''s true reservation price) and extraction (terms systematically exceeding it, taken by credible blocking threat)?',
    'Examine conceded terms across documented blocking episodes: do concessions track the blocker''s stated concerns and verifiable costs, or do they exceed any plausible reservation price (rule-of-law conditionality reversals, sectoral carve-outs unrelated to the blocked file)?',
    'Systematic above-reservation concessions would relocate this constraint toward the veto_trap_reading''s structure even within a sympathetic analysis, raising epsilon and introducing a fixed victim class this reading currently denies exists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(holdout_valence_boundary, conceptual, 'The valence boundary of holdout concessions: the exact location of the disagreement between this reading and its siblings.').

omega_variable(
    counterfactual_qmv_baseline,
    'Is the low extraction assessment robust to the choice of counterfactual baseline — a qualified-majority world with faster decisions but more imposed policy and defection, or an idealized consensus world with neither delay nor imposition?',
    'Model counterfactual decision latency, imposition rates, and compliance under extension of the post-Lisbon qualified-majority formula to the remaining unanimity domains, using the 2014-2017 transition as a natural experiment.',
    'Baseline choice shifts measured extraction materially: against the idealized baseline this arrangement looks purely costly; against the defection-prone baseline its costs look like purchases. This reading''s low epsilon presumes the realistic baseline, and the presumption should be made explicit wherever the number travels.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_qmv_baseline, empirical, 'Counterfactual sensitivity of the reading''s low-extraction verdict.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_council_unanimity__diplomatic_capital_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_c_tr_t0, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(eu_c_tr_t8, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement(eu_c_tr_t16, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 16, 0.17).
narrative_ontology:measurement(eu_c_tr_t24, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 24, 0.19).
narrative_ontology:measurement(eu_c_tr_t32, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 32, 0.21).
narrative_ontology:measurement(eu_c_tr_t40, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(eu_c_be_t0, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(eu_c_be_t8, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 8, 0.23).
narrative_ontology:measurement(eu_c_be_t16, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 16, 0.25).
narrative_ontology:measurement(eu_c_be_t24, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 24, 0.26).
narrative_ontology:measurement(eu_c_be_t32, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 32, 0.27).
narrative_ontology:measurement(eu_c_be_t40, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 40, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(eu_council_unanimity__diplomatic_capital_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_council_unanimity__diplomatic_capital_reading, resource_allocation).
narrative_ontology:affects_constraint(eu_council_unanimity__diplomatic_capital_reading, sovereignty_guarantor_reading).
narrative_ontology:affects_constraint(eu_council_unanimity__diplomatic_capital_reading, veto_trap_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'EU Council unanimity' decomposes into three structurally distinct claims per the epsilon-invariance principle. sovereignty_guarantor_reading is historically upstream (the founding rationale cited in treaty debates); this diplomatic_capital_reading occupies the middle position (the operational justification offered in Council practice); veto_trap_reading is the downstream challenger (the critique assembled from blocking episodes). Each story authors its own epsilon over the same standing arrangement: this file authors low epsilon (0.28) because it prices holdout concessions as legitimate bargaining; the siblings author their own values. All three files link one another through affects_constraints so contamination and drift propagate across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eu_council_unanimity__diplomatic_capital_reading, powerful, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
