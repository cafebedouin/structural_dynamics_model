% ============================================================================
% CONSTRAINT STORY: federation_membership_kernel__welfare_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_kernel__welfare_coordination_reading, []).

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
 *   constraint_id: federation_membership_kernel__welfare_coordination_reading
 *   human_readable: EU Free Movement as Welfare-System Coordination with Anti-Social-Dumping Enforcement
 *   domain: political economy/federalism/migration policy/welfare state theory
 *
 * SUMMARY:
 *   This story instantiates the welfare_coordination_reading of the
 *   federation_membership_kernel: free movement is real and enforced, but it
 *   operates through coordination of 27 national welfare systems rather than
 *   supranational harmonization, with the EU policing a social-dumping floor
 *   while member states keep welfare design autonomy. The epsilon referent is
 *   the standing arrangement under contest — the coordination regime as it
 *   actually operates (Regulation 883/2004, the Posted Workers Directive as
 *   revised in 2018, the A1 certificate system, the Court's service-freedom
 *   case law) — assessed by this reading's own lights, never the
 *   integrated-harmonized arrangement the integration_reading would build or
 *   the closed national systems the member_sovereignty_reading would defend.
 *   The expected structural delta is realized in the victim set: posted
 *   workers enter as targets via the cost-competition mechanism (their
 *   two-year home-country social-levy window is the product being sold),
 *   receiving-state labor markets face dual pressure from posted undercutting
 *   and permanent-migrant displacement, and sending states lose trained
 *   workers without fiscal compensation. The claim and the metrics are
 *   independent authored facts: tangled_rope is claimed because the structure
 *   demonstrably carries both a genuine coordination function and asymmetric
 *   extraction under active enforcement; the metric values describe observed
 *   operation, and the engine computes per-seat classifications from the
 *   structural data without reference to this claim.
 *
 * KEY AGENTS:
 *   - posted_workers: Primary target (powerless/constrained) — dispatched crews whose home-country pay and contribution levels are the price wedge being sold
 *   - cross_border_service_contractors: Primary beneficiary (organized/arbitrage) — captures the cost gap as contract margin across corridors
 *   - host_state_domestic_workforces: Target (organized/constrained) — bid against posted crews while carrying full domestic contribution packages
 *   - host_state_domestic_firms: Target (moderate/constrained) — tender against posting-based cost structures or adapt by posting themselves
 *   - receiving_state_governments: Mixed seat (institutional/constrained) — absorbs labor-market pressure while retaining welfare-design control
 *   - sending_state_governments: Mixed seat (institutional/constrained) — exports labor and training investment without fiscal compensation
 *   - mobile_permanent_migrants: Beneficiary (moderate/mobile) — portable entitlements across aggregated contribution records
 *   - eu_coordination_institutions: Agenda-setter (institutional/analytical) — drafts and polices the coordination framework
 *   - ecj: Agenda-setter (institutional/analytical) — fixes the interpretive balance between service freedom and labor protection
 *   - host_state_trade_unions: Excluded voice (organized/trapped) — lost strike leverage against posting in the Court's collective-action line
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_kernel__welfare_coordination_reading, 0.58).
domain_priors:suppression_score(federation_membership_kernel__welfare_coordination_reading, 0.55).
domain_priors:theater_ratio(federation_membership_kernel__welfare_coordination_reading, 0.29).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, theater_ratio, 0.29).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_kernel__welfare_coordination_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership_kernel__welfare_coordination_reading, "EU Free Movement as Welfare-System Coordination with Anti-Social-Dumping Enforcement").
narrative_ontology:topic_domain(federation_membership_kernel__welfare_coordination_reading, "political economy/federalism/migration policy/welfare state theory").

domain_priors:requires_active_enforcement(federation_membership_kernel__welfare_coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_kernel__welfare_coordination_reading, 'faebcbe6-5003-4db0-9934-f32a0b2f28a1').
narrative_ontology:cs_kernel_codification('faebcbe6-5003-4db0-9934-f32a0b2f28a1', formalized).
narrative_ontology:cs_authority_grounding('faebcbe6-5003-4db0-9934-f32a0b2f28a1', lineage).
narrative_ontology:cs_interpretation_layer_present('faebcbe6-5003-4db0-9934-f32a0b2f28a1').
narrative_ontology:cs_reading_relation('faebcbe6-5003-4db0-9934-f32a0b2f28a1', federation_membership_kernel__integration_reading, influences).
narrative_ontology:cs_reading_relation('faebcbe6-5003-4db0-9934-f32a0b2f28a1', federation_membership_kernel__member_sovereignty_reading, influences).
narrative_ontology:cs_axiom('faebcbe6-5003-4db0-9934-f32a0b2f28a1', foundational, welfare_design_autonomy_is_constitutional).
narrative_ontology:cs_axiom_status(welfare_design_autonomy_is_constitutional, holdable).
narrative_ontology:cs_axiom_grounding('faebcbe6-5003-4db0-9934-f32a0b2f28a1', welfare_design_autonomy_is_constitutional, conventional).
narrative_ontology:cs_axiom('faebcbe6-5003-4db0-9934-f32a0b2f28a1', foundational, anti_dumping_enforcement_preserves_market_integration).
narrative_ontology:cs_axiom_status(anti_dumping_enforcement_preserves_market_integration, holdable).
narrative_ontology:cs_axiom_grounding('faebcbe6-5003-4db0-9934-f32a0b2f28a1', anti_dumping_enforcement_preserves_market_integration, instrumental).
narrative_ontology:cs_reference_frame('faebcbe6-5003-4db0-9934-f32a0b2f28a1', coordinated_national_welfare_settlement).
narrative_ontology:cs_drift_state('faebcbe6-5003-4db0-9934-f32a0b2f28a1', post_enlargement_post_laval_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('faebcbe6-5003-4db0-9934-f32a0b2f28a1', '').
narrative_ontology:cs_kernel_id(federation_membership_kernel__welfare_coordination_reading, federation_membership_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, cross_border_service_contractors).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, sending_state_governments).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, mobile_permanent_migrants).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, posted_workers).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, host_state_domestic_workforces).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, host_state_domestic_firms).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, receiving_state_governments).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, receiving_state_governments).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, sending_state_governments).
narrative_ontology:constraint_vindicates(federation_membership_kernel__welfare_coordination_reading, subsidiarity_principle).
narrative_ontology:constraint_vindicates(federation_membership_kernel__welfare_coordination_reading, social_security_applicable_law_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Construction, transport, agricultural, and care workers dispatched by their employers to work in another member state while remaining formally employed under home-country contracts. Their pay and social contributions stay anchored to the sending country's lower cost level for up to twenty-four months, which is precisely what makes them commercially attractive to the firms that send them. Many rotate between sites in crews, live in employer-arranged accommodation, and depend on the dispatching firm for continued work; returning home usually means returning to unemployment, so the circuit continues even where conditions on site deteriorate.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, posted_workers, payer,
    powerless, biographical, constrained, continental).

% Construction groups, transport operators, agricultural contractors, and staffing agencies that organize labor across borders. The twenty-four-month window during which workers remain under home-country social security, confirmed by A1 certificates of applicable law, is the operating basis of their cost advantage when bidding in higher-wage markets. They file the certificates, build multi-layer subcontracting chains, and rotate crews to stay inside the window; shifting posting flows between corridors is routine commercial flexibility.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, cross_border_service_contractors, beneficiary,
    organized, biographical, arbitrage, continental).

% Low- and mid-wage workers in receiving-state construction, transport, meat processing, agriculture, and care who compete for the same jobs as posted crews. Where a posted crew works under home-country contribution levels, the domestic wage-plus-contributions package becomes the expensive option, and sectoral collective agreements come under steady pressure. Union density in the affected trades has declined as subcontracting layers multiply.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, host_state_domestic_workforces, payer,
    organized, biographical, constrained, national).

% Receiving-state companies that pay full domestic wage and social-contribution packages and tender against firms staffing sites with posted crews. Some respond by setting up subsidiaries that post their own workers; others lose bids or withdraw from affected trades entirely. Their grievance centers on the cost gap created by the temporary-contribution window rather than on wage levels as such.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, host_state_domestic_firms, payer,
    moderate, biographical, constrained, national).

% Governments of higher-wage member states. They retain full control over welfare-system design — benefit levels, eligibility rules, financing — because harmonization never entered the settlement, and they collect long-run contributions from settled mobile workers. In the short run they absorb labor-market pressure from posting and carry the eventual health and pension costs of an aging workforce whose working years were taxed elsewhere.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, receiving_state_governments, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_kernel__welfare_coordination_reading, receiving_state_governments, beneficiary).

% Governments of lower-wage member states whose nationals staff much of the posting flow. They gain an export outlet for surplus labor, remittance inflows, and reduced headline unemployment, while publicly funded education and training walks abroad with each departing cohort and peripheral regions hollow out. They defended the posting window fiercely in the 2018 directive negotiations and resist any shortening of the social-security coordination period.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, sending_state_governments, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_kernel__welfare_coordination_reading, sending_state_governments, payer).

% EU citizens who relocate permanently rather than joining posting circuits — nurses, engineers, seasonal workers who settle. Coordination rules aggregate their contribution periods across states so pensions are not forfeited, prevent duplicate coverage, and make certain benefits exportable. They gain a continent-wide labor market with portable entitlements, and their children typically enter host welfare systems in full.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, mobile_permanent_migrants, beneficiary,
    moderate, generational, mobile, continental).

% The Commission's employment directorate and the European Labour Authority. They draft and police the coordination framework: monitoring A1 issuance practices, pursuing infringement proceedings against states that fail to transpose posting rules, coordinating cross-border labor inspections. Their mandate requires holding market-making and social-floor enforcement together, and their inspection capacity is thin relative to the volume of cross-border flows.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, eu_coordination_institutions, agenda_setter,
    institutional, generational, analytical, continental).

% The Court of Justice adjudicates collisions between service freedom and collective labor protections through preliminary references. Its judgments in the posting and collective-action line fixed how far a receiving state may impose terms on foreign service providers and how much leverage unions retain against undercutting; every subsequent legislative adjustment operates inside the space those rulings opened.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, ecj, agenda_setter,
    institutional, generational, analytical, continental).

% Unions in receiving-state construction and transport sectors. They litigated the collision between service freedom and collective action all the way to the Court of Justice and lost the operative question: blockading or refusing mixed crews was held disproportionate against service freedom. They retain consultative channels in Brussels, but the instrument that mattered — effective strike leverage against posting — sits outside the operative rules.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, host_state_trade_unions, excluded,
    organized, generational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_kernel__welfare_coordination_reading, cross_border_service_contractors).
narrative_ontology:fixing_cost_class(federation_membership_kernel__welfare_coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Social security coordination solves real collective-action problems for a continent of mobile workers: it assigns which national system covers a given worker, prevents double contributions and double coverage, aggregates insurance periods so careers spanning several states still yield pensions, and makes specified benefits exportable. The anti-social-dumping layer attempts to keep service circulation from eroding host labor standards by extending host mandatory terms to posted workers.
% TRANSFER_FUNCTION: Moves labor services across borders priced at sending-state cost bases (wages plus social levies) into receiving-state markets; moves the training investment financed by sending-state taxpayers into receiving-state labor forces without fiscal compensation; and moves welfare-design discretion to member states by keeping harmonization permanently off the table while enforcing the floor rules that make the arrangement politically survivable.
% ABSENT_VOICES: Posted workers themselves have no seat: their consent is mediated by the employers who file A1 applications, and no institution represents the posting circuit as experienced from inside a crew. Sending-state communities losing working-age cohorts are represented only by governments that also profit from the outflow. Host-state unions reached the table through litigation and lost the operative question there; their core demand was removed by judicial ruling rather than negotiated away.
% DISAPPEARANCE_RATIONALE: If the coordination regime vanished overnight, millions of aggregated pension records across 27 states would become unprocessable, cross-border service provision would halt pending new bilateral agreements, double-coverage disputes would flood national courts, and every member state would immediately re-erect border controls on labor — the single labor market would reorganize around a patchwork of bilateral treaties within months.
% FOUNDING_PROBLEM: Post-war reconstruction, and later single-market completion, required workers to move between states whose social security systems were strictly national: without coordination, mobile workers faced double contributions, forfeited coverage periods, and unexportable benefits; without anti-dumping rules, unrestricted service freedom would let the lowest-cost labor standards undercut every other member state's wage structure.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties: OECD and ILO analyses document persistent intra-EU wage and social-contribution differentials large enough to sustain posting flows; host-state labor inspectorates and the European Labour Authority's own reporting document continuing fraud in posting chains (letterbox companies, fictitious A1 certificates); academic labor economics quantifies the earnings gap between posted and local workers doing equivalent work. The docket of posting-related preliminary references before the Court of Justice independently attests that the underlying problem has not been solved.
narrative_ontology:disappearance_verdict(federation_membership_kernel__welfare_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_kernel__welfare_coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_kernel__welfare_coordination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership_kernel__welfare_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_kernel__welfare_coordination_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_kernel__welfare_coordination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_kernel__welfare_coordination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_kernel__welfare_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.58: the coordination layer performs real portability work, but the posting window converts a wage-and-levy differential into private contract margin at scale, and the training-investment transfer from sending states is uncompensated by design. Suppression is 0.55 and unscaled by context — it reflects the raw coercive machinery the arrangement needs: A1 verification, infringement actions, and above all the Court's collective-action line, which removed the unions' most effective defensive instrument. Accessibility_collapse is 0.35 because alternatives persist (EEA-style arrangements, bilateral treaties, unilateral openness) even though no member state can opt out of coordination while remaining inside the membership kernel. Resistance is 0.60 — the Laval and Viking strikes, the bruised 2018 directive negotiations, and exit votes driven partly by free movement are real, sustained pushback. Theater_ratio is 0.29: enforcement is genuine but thin, and a visible share of compliance activity (certificate paperwork, transposition formalities) functions as display while subcontracting-chain fraud persists. The temporal series run on one shared grid (T=0..54, years since the 1971 coordination regulation) with all three metrics authored at every point. Extractiveness climbs sharply after the 2004 enlargement opened large wage differentials (T=33), peaks in the letterbox-firm era following the Court's collective-action rulings (T=40), and dips modestly at the 2018 directive revision (T=47) before resuming growth as posting spreads into care and platform work. The suppression_requirement series is authored deliberately: this story specifically tracks enforcement-capacity buildup (inspection bodies, the European Labour Authority, certificate verification), which rose monotonically across the interval — a maturing enforcement ratchet, not a static picture. Theater peaks alongside extractiveness at T=40, when rhetorical commitment to the social floor was highest relative to inspection capacity, then falls back as the 2018 revision added enforceable obligations.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the contractor seat the arrangement is the enabling infrastructure of a business model — coordination is what makes continental service provision possible, and the cost window is ordinary comparative advantage. From the posted-worker seat the same window is the reason their labor sells below the surrounding market: they are simultaneously the beneficiaries of the wage differential and the commodity through which it is captured. From the host-workforce and host-firm seats the arrangement operates as imposed cost competition they did not agree to and cannot individually escape. From the sending-government seat it is a safety valve with a hidden invoice; from the receiving-government seat it is autonomy preserved at the price of absorbed pressure. The agenda-setting seats (Commission, Court) experience the arrangement as a balance they maintain — which is precisely the experience the payer seats do not have. The engine derives these divergences from the structural data; nothing in the authored claim adjudicates among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations: cross_border_service_contractors (capture the cost gap directly), sending_state_governments (employment relief, remittances), mobile_permanent_migrants (portable entitlements). Victim declarations: posted_workers, host_state_domestic_workforces, host_state_domestic_firms, receiving_state_governments. The derivation maps beneficiaries toward low d and victims toward high d, modulated by exit: contractors hold arbitrage-grade exit (corridor-switching is their core competence) and sit nearest the beneficiary pole; posted workers are constrained (going home means unemployment) and sit far toward the target pole; trapped union leverage reinforces the suppression picture without altering d. One override is authored: the powerless seat (posted_workers) is set to d=0.72 rather than the near-full-target value the victim declaration alone would produce, because the derivation cannot see that posted workers voluntarily capture the wage differential — posting pays better than the home alternative, which is why the circuit persists; their target position is real but sits below full-target. The dual-positioned government seats are handled through secondary roles rather than overrides: sending_state_governments carry beneficiary with secondary payer (net position genuinely contested — see the brain_drain omega), and receiving_state_governments carry payer with secondary beneficiary (labor-market pressure borne, autonomy and long-run contributions collected).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — portability for mobile workers and a floor under wage competition — is live, so no mandatrophy is declared and none is resolved. The classification work here is preventive in both directions. Reading the arrangement as pure coordination (rope) would erase the posting cost wedge, the Laval-line removal of union leverage, and the uncompensated training transfer — the exact moves the arrangement's defenders make. Reading it as pure extraction (snare) would erase the real portability function that millions of permanently mobile citizens depend on and that no bilateral patchwork currently replicates. Tangled_rope holds both: the same A1 certificate that guarantees a nurse's pension aggregation also certifies the construction crew whose two-year levy window undercuts the site's domestic bidder. The temporal record guards against lifecycle misreading in turn: the 2018 extractiveness dip could be mistaken for successful reform, but the resumed rise at T=54 and the still-climbing suppression requirement indicate the extraction layer regenerating inside a strengthening enforcement shell rather than the coordination function displacing it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'Is the standing arrangement best read as coordination of autonomies (this file), as a constitutive citizenship right awaiting fuller integration (integration_reading), or as a bounded permission revocable by national capacity (member_sovereignty_reading)?',
    'Structural comparison across the three sibling stories: which element each reading relocates — the victim set composition, the direction of enforcement, the epsilon referent. This story places posted workers and host labor markets in the victim set and directs enforcement at dumping; the integration_reading relocates the violation to national exclusions; the member_sovereignty_reading relocates it to tolerated posting.',
    'Under the integration_reading, national welfare-conditioned exclusions become the extractive surface and epsilon on member-state barriers rises; under the member_sovereignty_reading, the posting tolerance itself becomes the violation of solidarity and the enforcement direction inverts. Classification of the same observable facts flips with the reading adopted.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Committer-frame omega: this constraint is one reading of federation_membership_kernel; sibling readings instantiate different constraints with different victim sets and epsilon referents.').

omega_variable(
    posting_abuse_share,
    'What share of posting is genuine temporary service provision between real establishments, versus disguised permanent relocation run through letterbox companies and fictitious A1 certificates?',
    'Cross-checked A1 certificate data against establishment-level payroll and inspection audits; European Labour Authority enforcement statistics; national labor-inspectorate fraud findings.',
    'If the abusive share is large, the measured extractiveness understates the arrangement''s actual operation and the theater component dominates enforcement; if small, the cost wedge largely reflects genuine comparative advantage and the coordination framing strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(posting_abuse_share, empirical, 'Composition of posting flows between legitimate service provision and constructed cost arbitrage.').

omega_variable(
    brain_drain_net_transfer,
    'Does the uncompensated transfer of training investment from sending states constitute net extraction once remittances, return migration, circular mobility, and knowledge circulation are counted?',
    'Fiscal-flow accounting comparing sending-state education and training expenditure embodied in emigrating cohorts against remittance inflows, returning-worker human capital, and diaspora trade effects.',
    'If the net loss is confirmed, the sending-state seat hardens from mixed toward target and the arrangement trends toward the snare boundary at that seat; if flows roughly compensate, sending_state_governments stand as genuine beneficiaries and the asymmetry concentrates on the receiving-side seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(brain_drain_net_transfer, empirical, 'Whether sending states are net losers or net gainers from the mobility settlement.').

omega_variable(
    laval_line_durability,
    'Is the Court''s service-freedom-over-collective-action balance stable doctrine, or contingent on a judicial composition and political climate that could shift?',
    'Trajectory of subsequent preliminary rulings touching posting, collective bargaining, and minimum-wage instruments; whether later case law narrows or extends the proportionality holding against strike leverage.',
    'A doctrinal reversal would cut the suppression profile substantially, restore host-side defensive capacity, and pull the arrangement toward the rope boundary; consolidation of the current line locks the current classification in place.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(laval_line_durability, conceptual, 'Durability of the judicial balance on which the arrangement''s suppressive architecture rests.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_kernel__welfare_coordination_reading, 0, 54).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(fede_tr_t15, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 15, 0.11).
narrative_ontology:measurement(fede_tr_t25, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 25, 0.16).
narrative_ontology:measurement(fede_tr_t33, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 33, 0.24).
narrative_ontology:measurement(fede_tr_t40, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 40, 0.33).
narrative_ontology:measurement(fede_tr_t47, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 47, 0.27).
narrative_ontology:measurement(fede_tr_t54, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 54, 0.29).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 0, 0.26).
narrative_ontology:measurement(fede_be_t15, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 15, 0.3).
narrative_ontology:measurement(fede_be_t25, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 25, 0.38).
narrative_ontology:measurement(fede_be_t33, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 33, 0.52).
narrative_ontology:measurement(fede_be_t40, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 40, 0.61).
narrative_ontology:measurement(fede_be_t47, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 47, 0.56).
narrative_ontology:measurement(fede_be_t54, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 54, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(fede_su_t15, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 15, 0.26).
narrative_ontology:measurement(fede_su_t25, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 25, 0.34).
narrative_ontology:measurement(fede_su_t33, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 33, 0.43).
narrative_ontology:measurement(fede_su_t40, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 40, 0.51).
narrative_ontology:measurement(fede_su_t47, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 47, 0.53).
narrative_ontology:measurement(fede_su_t54, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 54, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_kernel__welfare_coordination_reading, resource_allocation).
narrative_ontology:affects_constraint(federation_membership_kernel__welfare_coordination_reading, integration_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__welfare_coordination_reading, member_sovereignty_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'EU free movement' conflates three structurally distinct constraints instantiated by three readings of the federation_membership_kernel. This story (welfare_coordination_reading) authors epsilon for the coordination-of-autonomies arrangement as operated: genuine portability coordination plus a posting cost wedge, victims concentrated in posted crews and receiving-state labor markets. The integration_reading authors epsilon for the same membership commitment read as constitutive citizenship right, where the extractive surface is national exclusion rather than posting. The member_sovereignty_reading authors epsilon for the capacity-bounded variant, where tolerated posting is itself the violation. Each file carries its own stable epsilon, beneficiary/victim structure, and claimed type; this file links to both siblings via network.affects_constraints, and the upstream/downstream pressure between readings is documented in each file's kernel_context.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership_kernel__welfare_coordination_reading, powerless, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
