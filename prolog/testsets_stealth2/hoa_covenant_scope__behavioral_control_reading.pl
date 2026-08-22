% ============================================================================
% CONSTRAINT STORY: hoa_covenant_scope__behavioral_control_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hoa_covenant_scope__behavioral_control_reading, []).

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
 *   constraint_id: hoa_covenant_scope__behavioral_control_reading
 *   human_readable: HOA Covenant Behavioral-Control Regime (Behavioral-Control Reading)
 *   domain: property law/collective governance/urban planning
 *
 * SUMMARY:
 *   This file instantiates ONE reading of the hoa_covenant_scope kernel — the
 *   behavioral_control_reading — as a clean, epsilon-invariant constraint.
 *   Referent: the standing covenant regime of a mature common-interest
 *   community as that regime actually operates, assessed by this reading's
 *   own lights: a recorded declaration of covenants, conditions, and
 *   restrictions administered by an elected board that enforces aesthetic
 *   uniformity (paint palettes, roof styles, lawn and landscape standards,
 *   vehicle and trailer visibility) and behavioral conformity (signage bans,
 *   flag restrictions, decoration rules, use limitations) as a property-value
 *   maximization strategy. This reading treats the conformity content as the
 *   covenant's operative core and the maintenance content as background; the
 *   sibling readings re-weight that emphasis and are authored as separate
 *   constraints. Manifest victim terms map as follows: 'nonconformists'
 *   becomes nonconforming_homeowners; 'marginal_aesthetics' (tastes outside
 *   the enforced palette) is folded into that same group; the
 *   speech-suppression dimension (yard signs, flags) is carried by
 *   dissenting_expression_residents. KEY AGENTS (by structural relationship):
 *   - hoa_board_majority: Agenda setter (organized/constrained) — adopts and
 *   interprets the standard, levies fines, authorizes liens; lives under its
 *   own rules - conformist_majority: Primary beneficiary
 *   (organized/constrained) — receives the preferred streetscape as default;
 *   supplies the board's electoral base - board_aligned_homeowners: Secondary
 *   beneficiary (moderate/constrained) — faster approvals, warning-tier
 *   enforcement; defends the board politically - nonconforming_homeowners:
 *   Primary target (powerless/trapped) — bears citations, fines, lien
 *   threats; exit means selling into a covenant-bound market -
 *   dissenting_expression_residents: Target (moderate/constrained) — signage
 *   and flag displays trigger escalating enforcement; litigation is the
 *   costly exception path - renters_in_governed_homes: Cost-bearing non-voter
 *   (powerless/mobile) — bound by rules they cannot vote on; fines pass
 *   through landlords - realtors_marketing_the_community: Incidental
 *   beneficiary (moderate/arbitrage) — sells uniformity as a product feature
 *   across many communities - prospective_nonconforming_buyers: Excluded
 *   voice (moderate/mobile) — self-selects out before governance; objections
 *   never enter the record - state_legislature: Analytical observer
 *   (institutional/analytical) — writes the statutory envelope; hears both
 *   camps; enacts periodic carve-outs
 *
 * KEY AGENTS:
 *   - - hoa_board_majority: Agenda setter (organized/constrained) — adopts and interprets the standard, levies fines, authorizes liens; lives under its own rules
 *   - - conformist_majority: Primary beneficiary (organized/constrained) — receives the preferred streetscape as default; supplies the board's electoral base
 *   - - board_aligned_homeowners: Secondary beneficiary (moderate/constrained) — faster approvals, warning-tier enforcement; defends the board politically
 *   - - nonconforming_homeowners: Primary target (powerless/trapped) — bears citations, fines, lien threats; exit means selling into a covenant-bound market
 *   - - dissenting_expression_residents: Target (moderate/constrained) — signage and flag displays trigger escalating enforcement; litigation is the costly exception path
 *   - - renters_in_governed_homes: Cost-bearing non-voter (powerless/mobile) — bound by rules they cannot vote on; fines pass through landlords
 *   - - realtors_marketing_the_community: Incidental beneficiary (moderate/arbitrage) — sells uniformity as a product feature across many communities
 *   - - prospective_nonconforming_buyers: Excluded voice (moderate/mobile) — self-selects out before governance; objections never enter the record
 *   - - state_legislature: Analytical observer (institutional/analytical) — writes the statutory envelope; hears both camps; enacts periodic carve-outs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hoa_covenant_scope__behavioral_control_reading, 0.47).
domain_priors:suppression_score(hoa_covenant_scope__behavioral_control_reading, 0.6).
domain_priors:theater_ratio(hoa_covenant_scope__behavioral_control_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, extractiveness, 0.47).
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hoa_covenant_scope__behavioral_control_reading, snare).
narrative_ontology:human_readable(hoa_covenant_scope__behavioral_control_reading, "HOA Covenant Behavioral-Control Regime (Behavioral-Control Reading)").
narrative_ontology:topic_domain(hoa_covenant_scope__behavioral_control_reading, "property law/collective governance/urban planning").

domain_priors:requires_active_enforcement(hoa_covenant_scope__behavioral_control_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hoa_covenant_scope__behavioral_control_reading, '0ce03f2c-cb60-4f19-a1e8-e6c6bc12e7cc').
narrative_ontology:cs_kernel_codification('0ce03f2c-cb60-4f19-a1e8-e6c6bc12e7cc', fixed_text).
narrative_ontology:cs_authority_grounding('0ce03f2c-cb60-4f19-a1e8-e6c6bc12e7cc', extraction).
narrative_ontology:cs_interpretation_layer_present('0ce03f2c-cb60-4f19-a1e8-e6c6bc12e7cc').
narrative_ontology:cs_reading_relation('0ce03f2c-cb60-4f19-a1e8-e6c6bc12e7cc', hoa_covenant_scope__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('0ce03f2c-cb60-4f19-a1e8-e6c6bc12e7cc', hoa_covenant_scope__extraction_reading, influences).
narrative_ontology:cs_axiom('0ce03f2c-cb60-4f19-a1e8-e6c6bc12e7cc', foundational, aesthetic_uniformity_protects_property_values).
narrative_ontology:cs_axiom_status(aesthetic_uniformity_protects_property_values, holdable).
narrative_ontology:cs_axiom_grounding('0ce03f2c-cb60-4f19-a1e8-e6c6bc12e7cc', aesthetic_uniformity_protects_property_values, empirically_contingent).
narrative_ontology:cs_axiom('0ce03f2c-cb60-4f19-a1e8-e6c6bc12e7cc', foundational, recorded_covenant_consent_authorizes_conduct_regulation).
narrative_ontology:cs_axiom_status(recorded_covenant_consent_authorizes_conduct_regulation, holdable).
narrative_ontology:cs_axiom_grounding('0ce03f2c-cb60-4f19-a1e8-e6c6bc12e7cc', recorded_covenant_consent_authorizes_conduct_regulation, conventional).
narrative_ontology:cs_reference_frame('0ce03f2c-cb60-4f19-a1e8-e6c6bc12e7cc', aesthetic_uniformity_value_regime).
narrative_ontology:cs_drift_state('0ce03f2c-cb60-4f19-a1e8-e6c6bc12e7cc', contemporary_appraisal_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0ce03f2c-cb60-4f19-a1e8-e6c6bc12e7cc', '').
narrative_ontology:cs_kernel_id(hoa_covenant_scope__behavioral_control_reading, hoa_covenant_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__behavioral_control_reading, conformist_majority).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__behavioral_control_reading, board_aligned_homeowners).
narrative_ontology:constraint_victim(hoa_covenant_scope__behavioral_control_reading, nonconforming_homeowners).
narrative_ontology:constraint_victim(hoa_covenant_scope__behavioral_control_reading, dissenting_expression_residents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__behavioral_control_reading, realtors_marketing_the_community).
narrative_ontology:constraint_victim(hoa_covenant_scope__behavioral_control_reading, renters_in_governed_homes).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Elected owners who adopt and amend the architectural guidelines, hear violation cases, levy fines, and authorize liens. They decide which deviations get cited and which are overlooked, and they control the association's legal posture. Their authority rests on the recorded covenants and the state statutes governing common-interest communities; they cannot opt out of the rules they administer without selling their own homes.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, hoa_board_majority, agenda_setter,
    organized, biographical, constrained, local).

% Owners whose houses, landscaping, and habits already match the recorded standard. They pay the same assessments as everyone else, rarely interact with the enforcement process, and receive the streetscape they prefer as the default condition of the neighborhood. Selling is possible but means leaving the community and its price history behind.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, conformist_majority, beneficiary,
    organized, biographical, constrained, local).

% Owners close to the sitting board — friends, former officers, campaign supporters — whose requests move quickly through architectural review and whose visible lapses tend to draw warnings rather than fines. They defend the board at meetings and supply the votes that keep amendment thresholds out of reach.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, board_aligned_homeowners, beneficiary,
    moderate, biographical, constrained, local).

% Owners whose property diverges from the standard — unpainted trim, boat trailers, xeriscaped front yards, cluttered driveways. They receive citation letters, accumulate fines, and face lien or foreclosure threats if balances grow. Appealing means appearing before the same board that cited them; selling means giving up the home on the market's timetable, and nearby alternatives mostly carry similar covenants.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, nonconforming_homeowners, payer,
    powerless, biographical, trapped, local).

% Residents who want to post yard signs, fly seasonal or political flags, or decorate in ways the guidelines prohibit. Each display draws a violation notice; repeated displays escalate to fines. Their options are taking the display down, litigating where state law protects specific categories, or moving.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, dissenting_expression_residents, payer,
    moderate, biographical, constrained, local).

% Tenants in covenant-bound houses live under the same exterior and conduct rules but hold no vote in any association election and appear in no governance forum. Lease turnover is their only exit, and landlords pass through any fines charged against the property.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, renters_in_governed_homes, payer,
    powerless, immediate, mobile, local).
narrative_ontology:stakeholder_secondary_role(hoa_covenant_scope__behavioral_control_reading, renters_in_governed_homes, excluded).

% Agents who list homes in the community sell the managed appearance as a feature: mowed lawns, matched roofs, no visible clutter. Uniformity shortens their pitch and supports comparable pricing. They operate across many communities and can steer clients toward or away from covenant-heavy neighborhoods.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, realtors_marketing_the_community, beneficiary,
    moderate, immediate, arbitrage, regional).

% House-hunters who would want to keep a pickup truck, paint a door teal, or run a home business with visible signage. They screen themselves out during the showing stage and never enter the association's meetings or elections, so their objections exist nowhere in the community's decision record.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, prospective_nonconforming_buyers, excluded,
    moderate, immediate, mobile, regional).

% The state body that writes the statutes governing common-interest communities — fine caps, flag protections, sign-code carve-outs, solar-access exemptions, foreclosure limits. It hears testimony from boards and from owner coalitions, commissions studies, and periodically amends the statutory envelope the covenants operate inside.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, state_legislature, observer,
    institutional, generational, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hoa_covenant_scope__behavioral_control_reading, conformist_majority).
narrative_ontology:fixing_cost_class(hoa_covenant_scope__behavioral_control_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Pre-commits every lot in the community to one recorded standard of exterior appearance and resident conduct, so decisions with visible spillover — paint, landscaping, parking, signage, decorations — are settled by prior rule and board interpretation instead of house-by-house negotiation or neighbor-versus-neighbor litigation.
% TRANSFER_FUNCTION: Moves compliance labor, fine payments, and forgone expressive and lifestyle choices from cited households into a uniformly maintained streetscape administered by the board; moves interpretive discretion over the recorded standard into board hands, along with the fine revenue that enforcement generates.
% ABSENT_VOICES: Prospective buyers who would violate the standard self-select out before any meeting or election and appear nowhere in the community's decision record; renters live under the rules with no vote; future owners are bound by amendments passed after any given sale. Dissent enters only through the board's own hearing process, which the board staffs.
% DISAPPEARANCE_RATIONALE: If the conformity enforcement vanished overnight, visible divergence would accumulate within seasons — paint, parking, signage, yard use — fine revenue would stop, the architectural-review apparatus would idle, and the community would reorganize around either voluntary norms or a freshly recorded standard, with the conformist majority likely attempting rapid re-recording.
% FOUNDING_PROBLEM: Mid-century mass developers needed to assure early buyers in tract subdivisions that the advertised neighborhood character would persist — that a neighbor's choices could not degrade the look the brochure promised — and mortgage insurers required standardized collateral conditions before backing loans in these subdivisions.
% FOUNDING_PROBLEM_CORROBORATION: Period artifacts outside any benefiting party attest the founding problem was real at creation: FHA underwriting manuals and developer marketing archives explicitly tied mortgage insurance and sales velocity to recorded restrictions. Whether the problem remains live is attested differently by outsiders: hedonic-pricing researchers report mixed-to-negligible covenant value premiums, while state legislative hearings record boards asserting continued necessity and owner coalitions disputing it. No outside source attests the conformity mandate as straightforwardly live.
narrative_ontology:disappearance_verdict(hoa_covenant_scope__behavioral_control_reading, world_rearranges).
narrative_ontology:founding_problem_status(hoa_covenant_scope__behavioral_control_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hoa_covenant_scope__behavioral_control_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hoa_covenant_scope__behavioral_control_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hoa_covenant_scope__behavioral_control_reading, 0.47, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hoa_covenant_scope__behavioral_control_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hoa_covenant_scope__behavioral_control_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hoa_covenant_scope__behavioral_control_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.47 sits mid-band: the arrangement takes real, recurring value from identifiable households — fine revenue, compelled remediation, forgone expression — but the taken amounts are bounded by state fine caps and by the fact that most owners never interact with enforcement. Suppression 0.60 reflects a two-layer mechanism: a structural layer (recorded covenants that run with the land, architectural pre-approval, escalating fines, lien and foreclosure threats — roughly 60% of the force) and an internalized layer (neighbor-watching norms and conformity socialization that keep most deviation off the books entirely — roughly 40%); the split is carried as an omega rather than forced into the scalar. Theater_ratio 0.28: inspection rounds, violation hearings, and annual meetings partly perform diligence that ratifies pre-existing preferences, but the fine and lien machinery is functionally real. Accessibility_collapse 0.52: inside the community, alternatives to compliance collapse almost completely; outside it, exit exists but is blunted because the regional ownership stock is heavily covenant-bound, so selling usually trades one recorded standard for another. Resistance 0.55: appeals, demand letters, slate-election attempts, and periodic statutory carve-outs (flag and sign protections, solar access) show organized pushback without overturning the regime. The three temporal series share one seven-point grid (years 0-54 since recording, so every tracked metric is authored at every examined point); suppression_requirement is tracked deliberately because the enforcement picture is not static — volunteer committees gave way to professional management companies, published fine schedules, and routine lien practice, a documented enforcement-capacity ratchet. All values are retrospective structural estimates from the documented record, not instrument readings. Claim/metric independence: claimed_type snare is authored from this reading's structural verdict — the coordination story is real but thin, enforcement scope is expansive and reaches subjective judgment and expression, identifiable households bear targeted costs, and persistence depends on active enforcement machinery — while the metrics are authored as descriptive estimates; the engine computes per-seat types and any divergence is signal. Fixing-cost note supporting the receipt surface: amendment typically requires supermajorities of two-thirds to eighty percent of owners, dissolution is harder still, and the voters who would have to approve removal are the people the arrangement serves — hence prohibitive. Coordination-type note: identity_coordination is declared because the operative function is membership-boundary maintenance (what kind of community this is, who fits); the gaming risk flagged for this type — identity framing excusing asymmetric extraction — is exactly what the payer declarations and the extractiveness value carry openly here.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat experiences the covenant as the community's constitution — the thing that keeps the neighborhood worth what they paid — and computes a coordination-dominant picture. The conformist majority computes mild net benefit: they pay assessments and receive the streetscape they prefer as the default. The payer seats compute the opposite: nonconforming homeowners experience discretionary citation power aimed at them personally, with appeal routes that terminate at the citing board; dissenting-expression residents experience viewpoint-contingent enforcement; renters experience rules without any voice. Same nominal standing, divergent outcomes: a board-aligned owner and a cited nonconformer hold identical deeds, but alignment buys faster approvals and warning-tier treatment, so equal global standing resolves to different effective positions through constraint-specific factors (relationship to the board, visibility of deviation, taste distance from the standard). The engine computes these per-seat classifications from the structural data; this story does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations put the conformist majority and the board-aligned bloc near the beneficiary end (low d): the standard subsidizes their preferences at near-zero personal cost. Payer declarations put nonconforming homeowners near the full-target end, amplified by trapped exit — equity timing, age, and the covenant-bound character of nearby alternatives make departure costly, and every buyer inherits the same instrument. Dissenting-expression residents are targets with somewhat better exit (constrained rather than trapped). Renters bear pass-through costs, but lease-turnover mobility damps their effective position toward symmetric. Realtors benefit incidentally with arbitrage-grade exit across communities; they appear on the stakeholder surface as a second-order beneficiary while the base_properties beneficiary array stays faithful to the reading's declared primary beneficiaries. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled — by directionality and by the community's local spatial scope, which keeps verification-amplification modest compared to continental or universal constraints. Coalition caveat: individually powerless nonconformers could in principle capture the board by slate election — the arrangement's stability rests on their collective-action failure, sustained by dispersed and heterogeneous grievances, fear of retaliatory selective enforcement, and owner apathy; feasible coalition politics is the main structural threat to the current asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding mandate — guaranteed neighborhood character for lender-insured tract sales — is historically corroborated but externally contested today: hedonic evidence for covenant value premiums is mixed, while the enforcement apparatus has grown. This story does not declare mandatrophy resolved: the mandate still commands live beneficiary allegiance (the conformist majority re-elects boards on it), so the arrangement is not maintained by inertia alone, and its enforcement remains functional rather than theatrical. The snare classification matters because it blocks the natural mislabel — reading the conformity regime as the coordination reading's maintenance arrangement — while the contested founding status blocks the opposite error of declaring the mandate dead. Watch item: if the value-premium empirics resolve negative, the mandate's live status flips, and the arrangement should be re-examined for extraction_reading dominance or piton drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story authors one reading of the hoa_covenant_scope kernel; would instantiating the coordination_reading or extraction_reading instead yield a different type and epsilon for the same recorded covenants?',
    'Comparative generation of the sibling readings over the same referent with per-seat classification comparison; divergence in computed type across readings marks the reading-dependence of the verdict.',
    'If the coordination reading computes rope-like with low epsilon, the behavioral-control epsilon is reading-specific rather than covenant-intrinsic; if the extraction reading computes higher epsilon, this reading sits between its siblings on the extraction axis and the corpus gains an ordering of the kernel''s readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed epsilon over a shared kernel referent; sibling readings are other constraints, not parts of this one.').

omega_variable(
    uniformity_value_premium_empirics,
    'Does enforced aesthetic uniformity actually produce measurable property-value premiums relative to otherwise similar uncovenanted neighborhoods?',
    'Hedonic pricing studies comparing covenant-bound and non-covenanted subdivisions, controlling for age, location, school district, and amenity package.',
    'A null or negative premium collapses the maximization rationale, stripping the arrangement''s legitimizing premise and pushing it toward pure preference coercion; a robust premium would partially rehabilitate the founding mandate and damp the snare-side reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(uniformity_value_premium_empirics, empirical, 'Whether the value-maximization premise survives hedonic evidence.').

omega_variable(
    suppression_structural_vs_internalized,
    'How much of nonconformist acquiescence is produced by the recorded-rule, fine, and lien machinery versus internalized conformity norms and neighbor surveillance?',
    'Post-exit attitude surveys of former residents and natural experiments in communities where state law voided specific rule categories: persistence of conformity preferences after the enforcement mechanism is removed indicates internalization.',
    'If substantially internalized, the scalar understates total suppressive force — departing residents carry the standard with them and the constraint outlives its enforcement budget; if mostly structural, statutory rollback would release suppressed expression quickly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Split of suppressive force between legal machinery and internalized norms.').

omega_variable(
    nonconformist_coalition_feasibility,
    'Can the dispersed nonconforming homeowners convert individual weakness into board-capturing election slates, and what sustains their current collective-action failure?',
    'Comparative case history of association recalls and slate elections; turnout and retaliation data from communities where challenger slates ran.',
    'Feasible coalition politics would cap the board''s enforcement discretion and pull the arrangement toward negotiated standards; persistent failure entrenches the current asymmetry and supports the snare-side reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nonconformist_coalition_feasibility, empirical, 'Whether the targets can aggregate into counter-power.').

omega_variable(
    consent_scope_for_expressive_domains,
    'Does purchase-time consent to a recorded covenant legitimately extend to regulating political expression, signage, and lifestyle conduct, or only to physical-maintenance externalities?',
    'State statutory carve-outs (flag protections, sign codes, solar access) and case law testing covenant reach against constitutional and statutory floors; doctrinal analysis of consent theory applied to covenants running with the land.',
    'If expressive domains fall outside legitimate consent scope, the speech-suppression component loses its authorization structure and the enforcement scope contracts regardless of board preference, lowering both suppression and extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_scope_for_expressive_domains, conceptual, 'Normative scope of covenant consent over expression and lifestyle.').

omega_variable(
    selective_enforcement_rate,
    'What fraction of citations reflect the recorded standard applied uniformly versus board discretion favoring aligned households?',
    'Audit of citation records against guideline violations observable from public rights-of-way, cross-referenced with indicators of board alignment.',
    'High selectivity concentrates the arrangement''s costs on the unaligned and strengthens the board-power component of the story; uniform application would recast fines as a neutral pricing instrument and soften the target-side directionality.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(selective_enforcement_rate, empirical, 'Prevalence of discretionary and selective enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hoa_covenant_scope__behavioral_control_reading, 0, 54).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hoa_behavioral_control_tr_t0, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(hoa_behavioral_control_tr_t9, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 9, 0.14).
narrative_ontology:measurement(hoa_behavioral_control_tr_t18, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 18, 0.17).
narrative_ontology:measurement(hoa_behavioral_control_tr_t27, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 27, 0.2).
narrative_ontology:measurement(hoa_behavioral_control_tr_t36, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 36, 0.23).
narrative_ontology:measurement(hoa_behavioral_control_tr_t45, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 45, 0.26).
narrative_ontology:measurement(hoa_behavioral_control_tr_t54, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 54, 0.28).

% Extraction over time
narrative_ontology:measurement(hoa_behavioral_control_be_t0, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(hoa_behavioral_control_be_t9, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 9, 0.33).
narrative_ontology:measurement(hoa_behavioral_control_be_t18, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 18, 0.36).
narrative_ontology:measurement(hoa_behavioral_control_be_t27, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 27, 0.39).
narrative_ontology:measurement(hoa_behavioral_control_be_t36, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 36, 0.42).
narrative_ontology:measurement(hoa_behavioral_control_be_t45, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 45, 0.45).
narrative_ontology:measurement(hoa_behavioral_control_be_t54, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 54, 0.47).

% Suppression requirement over time
narrative_ontology:measurement(hoa_behavioral_control_su_t0, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(hoa_behavioral_control_su_t9, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 9, 0.4).
narrative_ontology:measurement(hoa_behavioral_control_su_t18, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 18, 0.45).
narrative_ontology:measurement(hoa_behavioral_control_su_t27, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 27, 0.5).
narrative_ontology:measurement(hoa_behavioral_control_su_t36, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 36, 0.54).
narrative_ontology:measurement(hoa_behavioral_control_su_t45, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 45, 0.57).
narrative_ontology:measurement(hoa_behavioral_control_su_t54, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 54, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hoa_covenant_scope__behavioral_control_reading, identity_coordination).
narrative_ontology:affects_constraint(hoa_covenant_scope__behavioral_control_reading, hoa_covenant_scope__coordination_reading).
narrative_ontology:affects_constraint(hoa_covenant_scope__behavioral_control_reading, hoa_covenant_scope__extraction_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the HOA covenant' conflates three structurally distinct claims about the same recorded instrument. This file isolates the behavioral-control claim (conformity enforcement serving value maximization, moderate epsilon); coordination_reading isolates the shared-maintenance and externality-resolution claim (low epsilon, rope-flavored); extraction_reading isolates the revenue-and-power-consolidation claim (high epsilon). Each carries its own epsilon, beneficiaries, and victims per the epsilon-invariance principle; the family is linked so contamination propagation and per-seat comparisons can be computed across readings. Upstream/downstream structure: the behavioral-control reading's enforcement-scope expansion (subjective aesthetic judgment, lifestyle restriction, speech suppression) supplies the discretionary machinery and fine infrastructure that the extraction reading describes riding on.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
