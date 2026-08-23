% ============================================================================
% CONSTRAINT STORY: fair_use_statutory_exception__market_licensing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_statutory_exception__market_licensing_reading, []).

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
 *   constraint_id: fair_use_statutory_exception__market_licensing_reading
 *   human_readable: Fair Use as Residual of Unmonetizable Uses (Market-Licensing Reading of Section 107)
 *   domain: legal/intellectual_property/information_economics
 *
 * SUMMARY:
 *   Under the market-licensing reading, the fair-use exception is governed by
 *   a single dominant test: any use that could have been licensed harms the
 *   market for licensed uses, and therefore fair use survives only where no
 *   licensing mechanism exists or could be constructed. The standing
 *   arrangement under contest — the referent of every metric here — is the
 *   regime this reading yields in practice: a fair use shrunk to de minimis
 *   and unmonetizable corners, with the entire reuse economy of quotation,
 *   commentary, sampling, preservation, and indexing routed into the payable
 *   set. This story is ONE reading of the fair_use_statutory_exception
 *   kernel; the sibling readings (transformative_right_reading,
 *   narrow_defense_reading) instantiate different constraints with different
 *   epsilon and are authored separately and linked via network edges. The
 *   epsilon authored here is invariant within this reading: it does not hedge
 *   across readings, average them, or describe the arrangements the siblings
 *   would produce. Claim and metrics are independent authored facts: the
 *   claimed type is tangled_rope because a determinate free/paid boundary
 *   does solve a genuine allocation-coordination problem while simultaneously
 *   extracting asymmetrically from everyone whose work touches existing
 *   culture; the metrics describe the heavily extractive, actively enforced,
 *   increasingly ritualized operation the reading actually yields. Where
 *   computed per-seat types diverge from the claim, that divergence is the
 *   datum.
 *
 * KEY AGENTS:
 *   - - major_rights_holder_conglomerates: Primary beneficiary (institutional/arbitrage) — collects fees, settlements, and judgments; funds enforcement and doctrine-shaping
 *   - - collective_rights_management_organizations: Secondary beneficiary (institutional/mobile) — takes percentage commissions on a widened licensable perimeter
 *   - - licensing_clearance_intermediaries: Secondary beneficiary (organized/mobile) — bills per clearance performed
 *   - - section_107_judicial_interpreters: Agenda setter (institutional/analytical) — administers the factor test that defines the payable set
 *   - - secondary_creators_remix_and_documentary: Primary target (organized/constrained) — absorbs clearance costs, re-cuts, and abandoned projects
 *   - - academic_and_scholarly_users: Target (organized/constrained) — diverts funds to permissions, drops reproductions
 *   - - libraries_archives_educational_institutions: Target (organized/constrained) — funds rights staff, negotiates blankets, absorbs takedowns
 *   - - platform_hosted_creators: Target (moderate/constrained) — faces automated matching, revenue redirection, blocking
 *   - - downstream_public_audience: Diffuse target (powerless/trapped) — bears narrowed cultural output with no procedural seat
 *   - - ip_law_scholarship: Analytical observer (analytical/analytical) — codes outcomes and tests the reading's claims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_statutory_exception__market_licensing_reading, 0.82).
domain_priors:suppression_score(fair_use_statutory_exception__market_licensing_reading, 0.75).
domain_priors:theater_ratio(fair_use_statutory_exception__market_licensing_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_statutory_exception__market_licensing_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_statutory_exception__market_licensing_reading, "Fair Use as Residual of Unmonetizable Uses (Market-Licensing Reading of Section 107)").
narrative_ontology:topic_domain(fair_use_statutory_exception__market_licensing_reading, "legal/intellectual_property/information_economics").

domain_priors:requires_active_enforcement(fair_use_statutory_exception__market_licensing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_statutory_exception__market_licensing_reading, '29cdccd5-aabe-4b2f-89b6-b712c5238d8b').
narrative_ontology:cs_kernel_codification('29cdccd5-aabe-4b2f-89b6-b712c5238d8b', fixed_text).
narrative_ontology:cs_authority_grounding('29cdccd5-aabe-4b2f-89b6-b712c5238d8b', lineage).
narrative_ontology:cs_interpretation_layer_present('29cdccd5-aabe-4b2f-89b6-b712c5238d8b').
narrative_ontology:cs_reading_relation('29cdccd5-aabe-4b2f-89b6-b712c5238d8b', fair_use_statutory_exception__transformative_right_reading, forecloses).
narrative_ontology:cs_reading_relation('29cdccd5-aabe-4b2f-89b6-b712c5238d8b', fair_use_statutory_exception__narrow_defense_reading, influences).
narrative_ontology:cs_axiom('29cdccd5-aabe-4b2f-89b6-b712c5238d8b', foundational, license_availability_implies_market_harm).
narrative_ontology:cs_axiom_status(license_availability_implies_market_harm, holdable).
narrative_ontology:cs_axiom_grounding('29cdccd5-aabe-4b2f-89b6-b712c5238d8b', license_availability_implies_market_harm, empirically_contingent).
narrative_ontology:cs_axiom('29cdccd5-aabe-4b2f-89b6-b712c5238d8b', secondary, fair_use_limited_to_unmonetizable_residual).
narrative_ontology:cs_axiom_status(fair_use_limited_to_unmonetizable_residual, holdable).
narrative_ontology:cs_axiom_grounding('29cdccd5-aabe-4b2f-89b6-b712c5238d8b', fair_use_limited_to_unmonetizable_residual, conventional).
narrative_ontology:cs_reference_frame('29cdccd5-aabe-4b2f-89b6-b712c5238d8b', licensing_market_value_preservation_frame).
narrative_ontology:cs_drift_state('29cdccd5-aabe-4b2f-89b6-b712c5238d8b', contemporary_post_goldsmith_streaming_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('29cdccd5-aabe-4b2f-89b6-b712c5238d8b', '').
narrative_ontology:cs_kernel_id(fair_use_statutory_exception__market_licensing_reading, fair_use_statutory_exception).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__market_licensing_reading, major_rights_holder_conglomerates).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__market_licensing_reading, collective_rights_management_organizations).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__market_licensing_reading, licensing_clearance_intermediaries).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, secondary_creators_remix_and_documentary).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, academic_and_scholarly_users).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, libraries_archives_educational_institutions).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, platform_hosted_creators).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, downstream_public_audience).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__market_licensing_reading, market_harm_factor_primacy).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__market_licensing_reading, copyright_incentive_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own large catalogs of recorded music, film, television, publishing, photography, and archival footage. Operate dedicated licensing divisions that price permissions for synchronization, reprint, excerpt, and merchandise use, and employ enforcement teams that send takedown notices and bring infringement actions. Revenue arrives as license fees, settlements, and statutory damages. When a proposed use goes unlicensed they characterize it as a sale they could otherwise have made. Their asset position lets them relocate catalogs, reprice permissions, or withdraw territories if any jurisdiction loosens the rules.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, major_rights_holder_conglomerates, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(fair_use_statutory_exception__market_licensing_reading, major_rights_holder_conglomerates, agenda_setter).

% Pool performance and reproduction rights across hundreds of thousands of members, issue blanket licenses to broadcasters, venues, streamers, and campuses, distribute royalties, and audit users. Income is a percentage commission on everything collected. Their business model depends on the perimeter of licensable uses staying wide, and they lobby accordingly.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, collective_rights_management_organizations, beneficiary,
    institutional, generational, mobile, continental).

% Clearance firms and rights agencies that locate owners, negotiate fees, and draft permission documents for producers, advertisers, publishers, and educators, billing per clearance performed or per project retainer. More complex rules and wider licensable boundaries mean more billable work.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, licensing_clearance_intermediaries, beneficiary,
    organized, biographical, mobile, national).

% Federal judges and appellate panels weigh the four statutory factors, hear litigants skewed toward repeat players, and issue the opinions that define where permission is owed. They collect nothing from outcomes either way; their exposure is reputational and doctrinal-consistency pressure from colleagues and higher courts.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, section_107_judicial_interpreters, agenda_setter,
    institutional, generational, analytical, national).

% Documentary filmmakers, video essayists, sampling musicians, visual collage artists, and podcasters whose work incorporates existing recordings, footage, photographs, and texts. Every incorporation carries either a clearance quote or a personal litigation-risk calculation; projects get re-cut, delayed, or shelved outright after rights-budget meetings. Guilds and legal-aid organizations pool advice and best-practice statements, but individual members carry project-level risk alone. Leaving the practice would mean abandoning their craft.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, secondary_creators_remix_and_documentary, payer,
    organized, biographical, constrained, global).

% Biographers, historians, critics, and scientists who quote and reproduce material in books, articles, and course materials. Universities maintain legal counsel and fair-use guidance; permissions budgets divert grant money from research; some projects drop reproductions or illustrations altogether rather than clear them. Scholarship that engages existing works cannot proceed without touching the boundary.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, academic_and_scholarly_users, payer,
    organized, biographical, constrained, global).

% Lend, preserve, digitize, exhibit, and teach with copyrighted holdings. They cannot stop acquiring copyrighted works without abandoning their mission, so they negotiate blanket licenses, fund dedicated rights-research positions, and absorb takedown demands against digital exhibits. Open-access acquisition and public-domain emphasis are partial refuges, not exits.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, libraries_archives_educational_institutions, payer,
    organized, generational, constrained, continental).

% Upload commentary, reaction, parody, gameplay, and educational video to large platforms. Automated content identification matches uploads against rights-holder reference files; matched revenue is redirected or videos blocked pending dispute. Appeal processes exist but structurally favor claimants with catalogs. Individual creators lack leverage; their practical choice is acceptance, alteration, or departure from the platform economy.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, platform_hosted_creators, payer,
    moderate, biographical, constrained, global).

% Read, watch, and listen. Experience fewer works that quote, remix, contextualize, or respond to existing culture, and encounter region-blocked or quietly withdrawn editions. They bear the outcome of every clearance decision and court ruling yet have no procedural voice in licensing terms or case records; their interest enters only through proxy institutions.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, downstream_public_audience, payer,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(fair_use_statutory_exception__market_licensing_reading, downstream_public_audience, excluded).

% Law professors, empirical researchers, and commentators who publish on how the doctrine operates, code case outcomes, model licensing-market effects, and testify in hearings. They hold no stake in collections and bear none of the clearance costs; their product is assessment of the arrangement itself.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, ip_law_scholarship, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_statutory_exception__market_licensing_reading, major_rights_holder_conglomerates).
narrative_ontology:fixing_cost_class(fair_use_statutory_exception__market_licensing_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Defines the boundary between uncompensated and compensated reuse: a prospective user can predict which incorporations of existing works trigger a fee, and catalog owners obtain a predictable demand schedule for permissions. Keeping free substitutes out of the option set keeps the licensing marketplace liquid and price-discoverable.
% TRANSFER_FUNCTION: Moves license fees, settlements, and judgment awards from users of expressive works to catalog owners and their collection agents; additionally moves withheld output — re-cut films, dropped illustrations, blocked uploads that never ship — from audiences to nobody, a loss booked on the paying side as avoided substitution.
% ABSENT_VOICES: Downstream audiences, hobbyist and unpaid creators, and future authors have no seat: litigation is funded by parties with balance-sheet stakes, licensing terms are negotiated firm-to-firm, and uses with no monetization path reach the record only through proxies such as library associations and filmmaker guilds.
% DISAPPEARANCE_RATIONALE: If the rule vanished overnight, much of the permissions market would collapse as users defaulted to unlicensed incorporation; catalog owners would reprice, lean harder on contracts and technical protection, and litigate the reconstruction of a payable boundary; secondary creators would regain a working margin of free reuse; platform matching systems and clearance departments would lose their object. Production, education, and moderation workflows across the information economy demonstrably depend on the rule's continued operation.
% FOUNDING_PROBLEM: Reconcile exclusive rights in expression with socially valuable reuse — criticism, commentary, news reporting, teaching, scholarship, parody — that per-use permission negotiations would chill; codified as 17 U.S.C. §107 after roughly 130 years of case law beginning with Folsom v. Marsh.
% FOUNDING_PROBLEM_CORROBORATION: Historical scholarship on the Folsom-to-§107 lineage corroborates the founding problem's original content. Corroboration that the problem is no longer solved under this reading comes from outside the beneficiary set: the Supreme Court's explicit refusal to equate commercial character with infringement in Campbell v. Acuff-Rose, empirical studies of clearance pricing exceeding what secondary users would ever pay, and documented project abandonments in library and filmmaker association filings. The reading's own proponents dispute the 'dead' finding, attesting that incentive erosion remains live — that dispute is the kernel contest itself.
narrative_ontology:disappearance_verdict(fair_use_statutory_exception__market_licensing_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_statutory_exception__market_licensing_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_statutory_exception__market_licensing_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fair_use_statutory_exception__market_licensing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_statutory_exception__market_licensing_reading, 0.82, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_statutory_exception__market_licensing_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fair_use_statutory_exception__market_licensing_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fair_use_statutory_exception__market_licensing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the licensability test converts essentially the entire reuse economy into a payable set: any use adjacent to an existing license class is presumed harmful, and new license classes (sync, clip, AI training) extend the boundary outward continuously. Suppression is high (0.75) because enforcement is largely preventive — automated matching, takedown defaults, clearance-or-abandon budgeting — operating before any judge weighs anything. Theater ratio is moderate and rising (0.35): fair-use review departments that rarely approve, boilerplate disclaimers, appeal workflows that process disputes without changing outcomes — compliance ritual accumulating around a substantively closed door. Accessibility_collapse (0.68): once a creator understands litigation exposure, the unpaid alternative effectively closes for them, though licensed-at-cost paths and genuinely unmonetizable corners remain open, so collapse is substantial but incomplete. Resistance (0.58): sustained doctrinal counter-campaign (the Campbell lineage, library and archivist advocacy, platform-scale pushback) without a frontal political assault on the reading itself. Temporal series run on one shared grid (T = years since 1965, points every 10) with all three metrics authored at every point; the dip in extractiveness at T=30 records the Campbell-era counter-turn, followed by renewed climb as streaming-era licensing infrastructure matured and recent market-focused rulings revived the reading. The downstream public is powerless and individually unorganized, but partial coalition representation (library alliances, filmmaker statements of best practices, platform-negotiated blankets) caps suppression below what fully isolated targets would suffer — coalition capacity is the main brake on this constraint and is itself contested terrain.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently by design of the structure. From the rights-holder seat the arrangement is orderly property administration: predictable compensation, liquid markets, enforceable boundaries — a coordination success it built and polices. From the payer seats the same structure is a tollgate with the exit doors removed: their practices intrinsically touch existing works, so the rule prices their craft itself. The judicial seat experiences neutral factor-balancing, unaware that repeat-play funding asymmetry tilts the input distribution it balances. The excluded public experiences nothing at all — works that never ship leave no trace to complain about. The engine derives this per-seat divergence from power, exit, and directional position; the authored claim adjudicates none of it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. Catalog conglomerates, collecting organizations, and clearance firms sit at the beneficiary pole (d near 0), with the conglomerates pushed furthest by arbitrage-grade exit — they can reprice, relocate, and restructure around any local loosening, so effective extraction damps toward subsidy for them. Payers with constrained exits — creators, scholars, libraries, platform uploaders — sit near the full-target pole (d near 1); their practices cannot route around the boundary. The downstream public is trapped and diffuse, maximizing the scope-amplified effective burden per capita of attention. The judicial administrator derives near-symmetric position (administers, collects nothing); the scholarly observer is analytical and outside the computation. Scope is global for the payers and beneficiaries alike, which scales verification difficulty and thus effective extraction upward for every target seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — sheltering socially valuable non-market reuse from permission friction — is inverted under this reading: the exception persists in name while covering precisely the uses no one would pay for, and the mismatch (status=dead crossed with verdict=world_rearranges) is deliberately authored as the capture signal the battery is built to detect, cross-checkable against the moderate-but-rising theater ratio. Classification as tangled_rope blocks both standard mislabels: calling the arrangement pure snare would erase the genuine residual coordination value (any property system needs a determinate free/paid frontier, and licensing markets do need demand-side liquidity); calling it rope would erase the asymmetry — concentrated collectors on one side, structurally unable-to-exit practitioners and a seatless public on the other. Persistence is maintained by classic Olsonian arithmetic: concentrated beneficiaries defend cheaply and visibly while diffuse losers cannot organize the purchase of reform, which is also why fixing_cost is prohibitive — whoever could fix it (Congress, a doctrinal supermajority of courts) faces concentrated, well-funded opposition against dispersed, weakly-represented gains.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_governance_contest,
    'Which reading of the fair-use kernel governs Section 107 application in practice — this market-licensing construal, the transformative-right construal, or the narrow-defense construal?',
    'Systematic coding of federal fair-use opinions by factor weighting and outcome, tracked decade over decade; legislative history if Congress ever amends the statute.',
    'Adoption of the transformative-right sibling would cut epsilon sharply for quotation, commentary, and indexing uses and shrink the beneficiary set to verbatim-substitution contexts; adoption of the narrow-defense sibling shifts the battleground to burden allocation while keeping the market criterion this reading supplies.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_governance_contest, conceptual, 'Committer-frame omega: this constraint is one reading of kernel fair_use_statutory_exception; each sibling reading instantiates a different constraint with its own epsilon and victim set.').

omega_variable(
    hypothetical_substitution_validity,
    'Does foregone-licensing revenue constitute real market harm when the specific use had no realistic buyer — that is, is the projected substitution economically actual?',
    'Demand-side studies comparing licensing price schedules with the demonstrated willingness to pay of actual secondary users; natural experiments around rulings that widened or narrowed the licensable set.',
    'If substitution is mostly unrealized projection, epsilon overstates real transfer and the arrangement reads as rent maintenance on paper markets; if substitution is real, part of the measured flow is genuine opportunity cost and the coordination framing strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hypothetical_substitution_validity, empirical, 'Whether the reading''s core harm claim tracks realized economics or projected rents.').

omega_variable(
    licenseability_boundary_extension,
    '''Could be licensed'' has no fixed edge — every new licensing technology (clip licensing, sync licensing, AI training-data deals) extends the boundary outward. Where does the licensable set terminate?',
    'Jurisprudential tracing of what courts treat as licensable, paired with market observation of newly minted license classes; AI-training negotiations as the current extension front.',
    'Epsilon is scope-contingent: each boundary extension mechanically converts formerly free uses into payable ones, so this reading''s extraction rises without any change in statutory text — the structural delta compounds over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(licenseability_boundary_extension, conceptual, 'Open-ended scope of the licensability test as the reading''s principal expansion mechanism.').

omega_variable(
    suppression_internalization_split,
    'Of the measured suppression, how much is structural (takedown regimes, litigation exposure, automated matching) versus internalized (creators conditioned to request permission for uses the doctrine would nominally allow)?',
    'Track creator behavior where enforcement slackens: if permission-seeking persists after litigation risk drops, the internalized share is substantial.',
    'Internalized suppression travels with creators after legal reform, so deregulatory remedies alone would recover less reuse than the structural measure suggests; the constraint''s suppressive force would then read partly as identity-lock dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_split, empirical, 'Structural versus internalized components of the constraint''s suppression.').

omega_variable(
    judicial_repeat_player_capture,
    'Have repeat-player dynamics — well-funded rights-holder litigants facing one-shot defendants — tilted judicial administration of the factor test toward this reading?',
    'Outcome statistics by litigant type and funding symmetry across fair-use litigation; comparison of pre-litigation takedown outcomes with post-judgment doctrine.',
    'Confirmed tilt supports the genealogy mismatch flagged in this story (dead founding problem combined with a world-rearranging verdict) and predicts further extraction accumulation absent structural intervention.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judicial_repeat_player_capture, empirical, 'Repeat-player tilt in the administrative seat.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_statutory_exception__market_licensing_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_use_market_licensing_tr_t0, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(fair_use_market_licensing_tr_t10, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement(fair_use_market_licensing_tr_t20, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement(fair_use_market_licensing_tr_t30, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement(fair_use_market_licensing_tr_t40, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 40, 0.26).
narrative_ontology:measurement(fair_use_market_licensing_tr_t50, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 50, 0.31).
narrative_ontology:measurement(fair_use_market_licensing_tr_t60, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 60, 0.35).

% Extraction over time
narrative_ontology:measurement(fair_use_market_licensing_be_t0, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(fair_use_market_licensing_be_t10, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 10, 0.67).
narrative_ontology:measurement(fair_use_market_licensing_be_t20, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 20, 0.7).
narrative_ontology:measurement(fair_use_market_licensing_be_t30, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 30, 0.66).
narrative_ontology:measurement(fair_use_market_licensing_be_t40, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 40, 0.72).
narrative_ontology:measurement(fair_use_market_licensing_be_t50, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 50, 0.77).
narrative_ontology:measurement(fair_use_market_licensing_be_t60, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 60, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(fair_use_market_licensing_su_t0, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 0, 0.44).
narrative_ontology:measurement(fair_use_market_licensing_su_t10, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(fair_use_market_licensing_su_t20, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 20, 0.56).
narrative_ontology:measurement(fair_use_market_licensing_su_t30, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 30, 0.61).
narrative_ontology:measurement(fair_use_market_licensing_su_t40, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 40, 0.66).
narrative_ontology:measurement(fair_use_market_licensing_su_t50, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 50, 0.71).
narrative_ontology:measurement(fair_use_market_licensing_su_t60, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 60, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_statutory_exception__market_licensing_reading, resource_allocation).
narrative_ontology:affects_constraint(fair_use_statutory_exception__market_licensing_reading, transformative_right_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__market_licensing_reading, narrow_defense_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__market_licensing_reading, dmca_notice_and_takedown_regime).

% DUAL FORMULATION NOTE:
% The colloquial label 'fair use' decomposes into at least three structurally distinct constraints — competing readings of one statutory kernel. This file instantiates the market_licensing_reading, with epsilon measured against the arrangement that reading produces (fair use reduced to unmonetizable residue; every licensable use payable). The transformative_right_reading sibling measures a lower epsilon for quotation, commentary, and indexing uses against its facilitative arrangement; the narrow_defense_reading sibling measures a moderately high epsilon against a burden-shifted defensive arrangement. Upstream/downstream: this reading historically supplied the substantive criterion (what counts as harm) that the narrow-defense reading operationalizes procedurally; the transformative reading arose as direct counter-doctrine in the Campbell lineage. Family links carried in affects_constraints; each member links to the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
