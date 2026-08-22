% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__democratic_participation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_kernel__democratic_participation_reading, []).

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
 *   constraint_id: speech_protection_kernel__democratic_participation_reading
 *   human_readable: Democratic-Participation Hierarchy of Speech Protection
 *   domain: constitutional law/political philosophy/communication rights
 *
 * SUMMARY:
 *   The democratic_participation_reading of the speech protection kernel
 *   holds that constitutional speech protection is strongest for political
 *   expression necessary for self-governance: political speech occupies a
 *   near-unconditioned core, while non-political expression — commercial,
 *   artistic, obscene, personal — sits in lower tiers where restriction is
 *   more readily permitted. This reading, descending from Meiklejohn through
 *   New York Times v. Sullivan, instantiates an internally hierarchical
 *   protection structure. Per the epsilon-invariance principle this file is
 *   ONE reading of the speech_protection_kernel, generated clean: the sibling
 *   readings (absolutist, harm_threshold, marketplace, dignity) are separate
 *   constraints with their own epsilon, beneficiary/victim structure, and
 *   type, linked through network.affects_constraints and the cs_structure
 *   reading relations. The referent of epsilon is the operated hierarchy
 *   itself — the standing arrangement under contest — assessed by this
 *   reading's own lights: the reading endorses the tiering principle while
 *   conceding that the operated hierarchy takes from identifiable seats.
 *   Claim and metrics are independently authored: the claimed type records my
 *   structural judgment of the operated arrangement; the metrics record its
 *   actual behavior. The temporal series run on one shared seven-point grid
 *   (1964-2026) so no metric's end-state is backfilled into earlier years.
 *
 * KEY AGENTS:
 *   - political_speakers_and_advocacy_groups: primary beneficiary (organized/constrained) — holds the top protection tier and the defamation shield; the seat the protection premium accrues to
 *   - voters_and_citizens: secondary beneficiary (moderate/trapped) — receives the protected information flow; bears diffuse costs of falsehood and unredressable attack
 *   - dissident_minority_movements: intended beneficiary (moderate/trapped) — the speakers the rationale was built for; their access to the top tier is contested in practice
 *   - nonpolitical_speakers: primary payer (moderate/constrained) — expression categorized outside the core; restricted more readily
 *   - targets_of_political_speech: payer and structurally excluded (moderate/trapped) — bear the redress denial the top tier requires; absent from the line-drawing conversation
 *   - courts_judiciary: agenda_setter (institutional/constrained) — administers the hierarchy and draws the category line
 *   - legislatures_regulators: dual-positioned (institutional/constrained) — prohibited on the political tier, licensed on the lower tiers
 *   - incumbent_officeholders: dual-positioned (powerful/arbitrage) — forbidden to censor, shielded in expression and spending; works the boundary most effectively
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__democratic_participation_reading, 0.5).
domain_priors:suppression_score(speech_protection_kernel__democratic_participation_reading, 0.65).
domain_priors:theater_ratio(speech_protection_kernel__democratic_participation_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, extractiveness, 0.5).
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__democratic_participation_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__democratic_participation_reading, "Democratic-Participation Hierarchy of Speech Protection").
narrative_ontology:topic_domain(speech_protection_kernel__democratic_participation_reading, "constitutional law/political philosophy/communication rights").

domain_priors:requires_active_enforcement(speech_protection_kernel__democratic_participation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__democratic_participation_reading, 'bd575236-a5b5-4f76-a5c4-b21137707c96').
narrative_ontology:cs_kernel_codification('bd575236-a5b5-4f76-a5c4-b21137707c96', fixed_text).
narrative_ontology:cs_authority_grounding('bd575236-a5b5-4f76-a5c4-b21137707c96', lineage).
narrative_ontology:cs_interpretation_layer_present('bd575236-a5b5-4f76-a5c4-b21137707c96').
narrative_ontology:cs_reading_relation('bd575236-a5b5-4f76-a5c4-b21137707c96', speech_protection_kernel__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('bd575236-a5b5-4f76-a5c4-b21137707c96', speech_protection_kernel__harm_threshold_reading, coexists_with).
narrative_ontology:cs_reading_relation('bd575236-a5b5-4f76-a5c4-b21137707c96', speech_protection_kernel__marketplace_reading, coexists_with).
narrative_ontology:cs_reading_relation('bd575236-a5b5-4f76-a5c4-b21137707c96', speech_protection_kernel__dignity_reading, coexists_with).
narrative_ontology:cs_axiom('bd575236-a5b5-4f76-a5c4-b21137707c96', foundational, self_governance_requires_maximal_political_protection).
narrative_ontology:cs_axiom_status(self_governance_requires_maximal_political_protection, holdable).
narrative_ontology:cs_axiom_grounding('bd575236-a5b5-4f76-a5c4-b21137707c96', self_governance_requires_maximal_political_protection, instrumental).
narrative_ontology:cs_axiom('bd575236-a5b5-4f76-a5c4-b21137707c96', secondary, civic_function_licenses_tiered_protection).
narrative_ontology:cs_axiom_status(civic_function_licenses_tiered_protection, holdable).
narrative_ontology:cs_axiom_grounding('bd575236-a5b5-4f76-a5c4-b21137707c96', civic_function_licenses_tiered_protection, conventional).
narrative_ontology:cs_reference_frame('bd575236-a5b5-4f76-a5c4-b21137707c96', sullivan_meiklejohnian_civic_core).
narrative_ontology:cs_drift_state('bd575236-a5b5-4f76-a5c4-b21137707c96', contemporary_post_citizens_united, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bd575236-a5b5-4f76-a5c4-b21137707c96', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, political_speakers_and_advocacy_groups).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, voters_and_citizens).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, dissident_minority_movements).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, nonpolitical_speakers).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, targets_of_political_speech).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, legislatures_regulators).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, incumbent_officeholders).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, legislatures_regulators).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, incumbent_officeholders).
narrative_ontology:constraint_vindicates(speech_protection_kernel__democratic_participation_reading, meiklejohnian_self_governance_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Candidates, parties, advocacy organizations, and media outlets covering politics. Their expression — campaign statements, criticism of officeholders, paid political advertising — receives the strongest shield available: government cannot restrict it absent the most compelling justification, and their statements about public figures carry a defamation shield that makes suits by those figures nearly unwinnable. They operate inside this polity's legal order; their speech concerns this polity, so relocating to another jurisdiction's speech regime is not a real alternative.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, political_speakers_and_advocacy_groups, beneficiary,
    organized, biographical, constrained, national).

% The audience the arrangement is nominally built to serve. They receive the protected flow of political information — dissent, criticism, campaign argument — and bear diffuse costs: exposure to political falsehood and attack advertising they have little legal recourse against, and the indirect cost of non-political expression being policed more readily. They cannot exit the information environment without leaving the polity.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, voters_and_citizens, beneficiary,
    moderate, biographical, trapped, national).

% Unpopular political movements and minority advocates — the speakers the rationale was originally built to protect. Their expression is politically central precisely because it challenges incumbents and majorities. In practice their access to the top tier is contested: prosecutors and courts sometimes reclassify their expression as conduct, threat, or incitement, so the shield they hold on paper is stronger than the one they hold in a courtroom.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, dissident_minority_movements, beneficiary,
    moderate, generational, trapped, national).

% Commercial advertisers, adult-content creators, artists, and ordinary people whose expression is categorized as outside the civic core. Restrictions on their expression face a lower bar: legislatures may regulate them where the political tier would forbid it. Some can litigate to have their speech reclassified as political — the boundary is movable — but category conversion is expensive, uncertain, and unavailable to expression whose character is fixed.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, nonpolitical_speakers, payer,
    moderate, biographical, constrained, national).

% Private citizens defamed during campaign cycles, people caught in public controversies, and communities targeted by political attack advertising. The speech that harms them sits in the protected tier, so their paths to redress are narrowed to near-closure: they must meet proof requirements only the best-resourced can attempt, and they have no seat where the protection line is drawn — they enter the doctrine as defendants or not at all.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, targets_of_political_speech, payer,
    moderate, immediate, trapped, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_kernel__democratic_participation_reading, targets_of_political_speech, excluded).

% The Supreme Court and lower federal courts. They decide what counts as political expression, which scrutiny tier applies, and where the boundary sits — every campaign-finance statute, content regulation, and defamation rule passes through their review. They are bound by their own precedent and the constitutional text; their discretion is the discretion to administer the hierarchy, not to dissolve it.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, courts_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Congress and state legislatures. They are forbidden from regulating the political tier — campaign speech, criticism of officeholders — no matter the harm shown. The same hierarchy licenses them to regulate the lower tiers: obscenity, commercial speech, and other non-political expression may be restricted where a public-interest case can be made. They hold regulatory power and regulatory prohibition in the same instrument.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, legislatures_regulators, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_kernel__democratic_participation_reading, legislatures_regulators, beneficiary).

% Sitting officeholders and their campaign committees. The arrangement forbids them to silence critics — the anti-censorship function operates directly against their administrative power. The same arrangement shields their own expression and spending at the top tier, lets them answer defamation suits under the same shield their critics hold, and lets them classify campaign expenditure as protected expression. They work the category boundary more effectively than any other seat.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, incumbent_officeholders, beneficiary,
    powerful, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_kernel__democratic_participation_reading, incumbent_officeholders, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_kernel__democratic_participation_reading, political_speakers_and_advocacy_groups).
narrative_ontology:fixing_cost_class(speech_protection_kernel__democratic_participation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the incumbent-censorship collective-action problem: by placing political expression beyond ordinary restriction, the arrangement guarantees that criticism of officeholders, advocacy, and campaign argument cannot be suppressed by those who hold power, and that voters encounter dissenting information. It also gives courts a principled axis — civic function — on which to sort protection claims.
% TRANSFER_FUNCTION: Moves legal protection differentially by civic category: immunity from restriction, heightened scrutiny against regulation, and the public-figure defamation shield flow to political expression and its funders; reduced protection flows to non-political expression; and the cost of unredressable reputational attack flows to the targets of protected political speech.
% ABSENT_VOICES: Targets of political speech have no seat where the protection line is drawn — they enter the doctrine as defendants or not at all, and their harm is priced as the cost of the core function. Unorganized non-political speakers likewise lack representation at the boundary: the line is drawn in litigation among courts, governments, and resourced litigants. Both groups would object that the hierarchy's costs are allocated without their participation.
% DISAPPEARANCE_RATIONALE: If the hierarchy vanished and protection equalized, every restriction on political expression would face ordinary scrutiny, the campaign-spending shield would evaporate, and defamation law would re-equilibrate — campaign practice, political advertising, and newsroom risk calculus would all reorganize within a few election cycles. If protection collapsed instead, incumbent censorship would return immediately. Either branch rearranges the world; no branch leaves it unchanged.
% FOUNDING_PROBLEM: Majoritarian governments entrench themselves by censoring opposition: sedition prosecutions, loyalty purges, and suppression of dissent were the observed pattern the doctrine was built against. Self-governing voters cannot judge if the state may silence the criticism and advocacy their judgments depend on.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: comparative-democracy monitoring (press-freedom and backsliding indices) documents governments restricting political speech as a standard entrenchment move; the historical record of sedition prosecutions is uncontested; and the reading's own critics — scholars attacking the doctrine's current operation as a shield for concentrated money and against speech targets — affirm that the founding problem of state censorship is real even as they dispute the hierarchy's present shape. No serious party attests the founding problem is solved.
narrative_ontology:disappearance_verdict(speech_protection_kernel__democratic_participation_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__democratic_participation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__democratic_participation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_protection_kernel__democratic_participation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__democratic_participation_reading, 0.5, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__democratic_participation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_kernel__democratic_participation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_kernel__democratic_participation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.50 from this reading's own lights: the tiering principle is endorsed, but the operated hierarchy takes from identifiable seats — non-political speakers face a lower protection bar than a neutral allocation would set, and targets of political speech are denied redress the civic function does not obviously require. Suppression (0.65) is structural, not carceral: the hierarchy is maintained by courts invalidating legislation and by doctrinal barriers that foreclose legislative and judicial alternatives for the payer seats. Theater (0.40): the core protection is real and load-bearing, but a growing share of the doctrine's operation is ceremonial — civic-core and marketplace rhetoric deployed to shield concentrated spending whose connection to voter self-governance is thin. Accessibility collapse (0.50): alternatives partially persist — the political boundary is litigable and some speakers convert categories — but for fixed-category speakers and for targets the alternatives have largely closed. Resistance (0.60): the boundary is under constant attack from legislatures, scholars, and reform movements, which is why enforcement effort rises across the series. The suppression_requirement series is authored because the story specifically tracks enforcement-capacity change: the doctrine's active invalidation practice matured and hardened from the Buckley era through the post-2010 campaign-finance cases, so the rising trajectory is the dynamic, not noise.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the agenda_setter seat (courts) and the beneficiary seats (political speakers, voters), the arrangement presents as the coordination structure that makes self-governance possible — the anti-censorship function is its whole point. From the payer seats, the same structure operates as an administered allocation in which their protection was the price of someone else's core: the non-political speaker sees a restriction license, the target sees a closed courthouse door. The incumbent seat should compute as the sharpest divergence: the same arrangement that forbids them to censor shields their spending — subsidized and bound in one seat. The engine computes these per-seat classifications from the structural data; the claimed type does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: political speakers sit near the beneficiary end — the premium accrues to them; voters sit low — they receive the flow and bear only diffuse costs; dissidents sit low in the reading's terms, with the practical caveat that their top-tier access is contested. Payers: nonpolitical_speakers and targets_of_political_speech carry high directionality — the hierarchy takes their protection-tier and their redress through the same structure that coordinates political discourse. The override on the powerful atom (d=0.3) corrects what the derivation cannot see: incumbents are declared beneficiaries, but the anti-censorship function binds them directly and their secondary payer position is invisible to a beneficiary-first derivation, so their structural position is meaningfully constrained as well as subsidized. Courts sit mid-range: they administer without capturing the premium. Legislatures are genuinely dual — prohibited on one tier, licensed on the other — which their role pair records.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents two mislabels. Reading the arrangement as pure coordination — 'it protects democracy, full stop' — would erase the identifiable payers: non-political speakers restricted through the same structure that coordinates political discourse, and targets denied redress. Reading it as pure extraction — 'protection is a cover for entrenched interests' — would erase the genuine and primary coordination function: the anti-censorship core is real, load-bearing, and was built against an observed problem that remains live. The founding problem is live (corroborated by the comparative backsliding record and by the reading's own critics), so mandatrophy is not resolved: the arrangement has not outlived its function. What has happened is drift, not obsolescence — the top tier now shields concentrated spending more robustly than it shields the dissident speech the rationale was built for, which is practice drift from the Meiklejohnian reference frame rather than a dead mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    speech_kernel_reading_index,
    'This constraint instantiates the democratic_participation_reading of the speech_protection_kernel — how would the constraint''s structure differ under the sibling readings (absolutist, harm_threshold, marketplace, dignity), and what would change if one displaced it?',
    'Compare the sibling constraint stories in the kernel family: each instantiates one reading with its own epsilon, beneficiary/victim structure, and type. The structural delta of this reading is the internal hierarchy — political speech maximal, non-political speech more readily restricted — which the absolutist sibling denies and the harm and dignity siblings replace with different conditions on protection.',
    'If a sibling reading displaced this one, the beneficiary/victim structure changes wholesale: the absolutist reading dissolves the hierarchy and its tiered payer seats; the harm-threshold and dignity readings re-key protection to harm and subordination rather than civic category, moving targets_of_political_speech out of the payer seat while making much currently protected political expression restrictible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(speech_kernel_reading_index, conceptual, 'Kernel membership and reading index for the speech_protection_kernel family.').

omega_variable(
    tiering_premise_disagreement,
    'The kernel contest turns on one structural element: whether legal protection may legitimately tier by civic function (this reading) or must be categorical, harm-conditioned, truth-serving, or dignity-conditioned. Which premise governs, and can any single framework hold more than one?',
    'Doctrinal evolution plus the sibling stories: if courts abandon category-tiering for harm- or dignity-conditioned tests, this reading''s constraint dissolves into the corresponding sibling. The reading_relations and axiom structure of the family encode which combinations are logically co-holdable within one framework.',
    'This reading forecloses the absolutist sibling within any single framework (near-categorical protection contradicts tiered restriction); it coexists with the harm, marketplace, and dignity readings. Displacement of the tiering premise would re-key epsilon and the victim set wholesale.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tiering_premise_disagreement, conceptual, 'Location of the kernel disagreement: the legitimacy of function-based tiering.').

omega_variable(
    political_category_boundary,
    'Who decides what counts as political expression necessary for self-governance, and does the administered boundary track civic function or the resources of the litigants?',
    'Doctrinal audit of boundary cases: compare how readily well-resourced litigants (campaign committees, media corporations) obtain top-tier classification versus unorganized speakers (dissidents, artists with political content), and whether the boundary''s movement over the interval correlates with litigant resources or with civic function.',
    'If the boundary tracks resources, the hierarchy''s extraction is higher than measured — the top tier operates as a purchasable good and the operated arrangement moves toward the extraction end; if it tracks civic function, the current assessment stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_category_boundary, empirical, 'Whether the political/non-political boundary is administered by function or by litigant power.').

omega_variable(
    target_redress_necessity,
    'Is denying full redress to targets of political speech an inherent cost of the coordination function (defamation liability would chill core speech), or overreach the function does not require?',
    'Comparative analysis across jurisdictions with lower defamation bars for political speech: measure whether political discourse quality, participation, and self-governance outcomes measurably degrade where targets'' redress is wider.',
    'If redress can be widened without degrading political discourse, part of the measured extraction is overreach rather than inherent coordination cost and the operated hierarchy''s extraction rises; if discourse degrades, the denial is inherent cost and the current assessment stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(target_redress_necessity, empirical, 'Whether the target-redress denial is inherent coordination cost or extractive overreach.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__democratic_participation_reading, 1964, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spk_dem_part_tr_t1964, speech_protection_kernel__democratic_participation_reading, theater_ratio, 1964, 0.15).
narrative_ontology:measurement_basis(spk_dem_part_tr_t1964, observed).
narrative_ontology:measurement(spk_dem_part_tr_t1976, speech_protection_kernel__democratic_participation_reading, theater_ratio, 1976, 0.18).
narrative_ontology:measurement_basis(spk_dem_part_tr_t1976, observed).
narrative_ontology:measurement(spk_dem_part_tr_t1988, speech_protection_kernel__democratic_participation_reading, theater_ratio, 1988, 0.22).
narrative_ontology:measurement_basis(spk_dem_part_tr_t1988, observed).
narrative_ontology:measurement(spk_dem_part_tr_t1998, speech_protection_kernel__democratic_participation_reading, theater_ratio, 1998, 0.26).
narrative_ontology:measurement_basis(spk_dem_part_tr_t1998, observed).
narrative_ontology:measurement(spk_dem_part_tr_t2010, speech_protection_kernel__democratic_participation_reading, theater_ratio, 2010, 0.33).
narrative_ontology:measurement_basis(spk_dem_part_tr_t2010, observed).
narrative_ontology:measurement(spk_dem_part_tr_t2018, speech_protection_kernel__democratic_participation_reading, theater_ratio, 2018, 0.36).
narrative_ontology:measurement_basis(spk_dem_part_tr_t2018, observed).
narrative_ontology:measurement(spk_dem_part_tr_t2026, speech_protection_kernel__democratic_participation_reading, theater_ratio, 2026, 0.4).
narrative_ontology:measurement_basis(spk_dem_part_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(spk_dem_part_be_t1964, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 1964, 0.3).
narrative_ontology:measurement_basis(spk_dem_part_be_t1964, observed).
narrative_ontology:measurement(spk_dem_part_be_t1976, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 1976, 0.34).
narrative_ontology:measurement_basis(spk_dem_part_be_t1976, observed).
narrative_ontology:measurement(spk_dem_part_be_t1988, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 1988, 0.37).
narrative_ontology:measurement_basis(spk_dem_part_be_t1988, observed).
narrative_ontology:measurement(spk_dem_part_be_t1998, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 1998, 0.4).
narrative_ontology:measurement_basis(spk_dem_part_be_t1998, observed).
narrative_ontology:measurement(spk_dem_part_be_t2010, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 2010, 0.46).
narrative_ontology:measurement_basis(spk_dem_part_be_t2010, observed).
narrative_ontology:measurement(spk_dem_part_be_t2018, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 2018, 0.48).
narrative_ontology:measurement_basis(spk_dem_part_be_t2018, observed).
narrative_ontology:measurement(spk_dem_part_be_t2026, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 2026, 0.5).
narrative_ontology:measurement_basis(spk_dem_part_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(spk_dem_part_su_t1964, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 1964, 0.35).
narrative_ontology:measurement_basis(spk_dem_part_su_t1964, observed).
narrative_ontology:measurement(spk_dem_part_su_t1976, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 1976, 0.42).
narrative_ontology:measurement_basis(spk_dem_part_su_t1976, observed).
narrative_ontology:measurement(spk_dem_part_su_t1988, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 1988, 0.48).
narrative_ontology:measurement_basis(spk_dem_part_su_t1988, observed).
narrative_ontology:measurement(spk_dem_part_su_t1998, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 1998, 0.52).
narrative_ontology:measurement_basis(spk_dem_part_su_t1998, observed).
narrative_ontology:measurement(spk_dem_part_su_t2010, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 2010, 0.6).
narrative_ontology:measurement_basis(spk_dem_part_su_t2010, observed).
narrative_ontology:measurement(spk_dem_part_su_t2018, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 2018, 0.63).
narrative_ontology:measurement_basis(spk_dem_part_su_t2018, observed).
narrative_ontology:measurement(spk_dem_part_su_t2026, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 2026, 0.65).
narrative_ontology:measurement_basis(spk_dem_part_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__democratic_participation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel__harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel__marketplace_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel__dignity_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'speech protection' covers five structurally distinct constraints — one per reading of the speech_protection_kernel. This file is the democratic_participation_reading: its epsilon (0.50) is the operated hierarchy assessed by this reading's own lights. The absolutist sibling would carry a different epsilon and no tiered payer set at all; the harm_threshold and dignity siblings re-key protection to harm and subordination, moving targets_of_political_speech out of the payer seat; the marketplace sibling grounds protection in truth-discovery rather than self-governance. Per the epsilon-invariance principle the readings are separate stories linked by network.affects_constraints rather than one story with a measurement parameter; the upstream/downstream pressure between them (this reading's civic-core category determines where marketplace and harm protections get their strongest application) is recorded in the cs_structure reading relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_protection_kernel__democratic_participation_reading, powerful, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
