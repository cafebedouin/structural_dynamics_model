% ============================================================================
% CONSTRAINT STORY: imposition_pathway_kernel__hybrid_cascade_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-06
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_pathway_kernel__hybrid_cascade_reading, []).

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
 *   constraint_id: imposition_pathway_kernel__hybrid_cascade_reading
 *   human_readable: Hybrid Cascade Pathway: Mandated Fringe Seeding Organic Commitment Climb (Meiji Reading)
 *   domain: historical sociology / state formation / commitment systems
 *
 * SUMMARY:
 *   This story instantiates the hybrid_cascade_reading of the
 *   imposition-pathway kernel: a state decree manufactures a mandatory
 *   adopting class among officials and military, and that artificial fringe -
 *   not a pre-existing disposition - becomes the vector through which the new
 *   commitment standard climbs organically to completion. The standing
 *   arrangement under contest, and the referent of epsilon, is the
 *   mandate-and-climb mechanism itself as operated in the Meiji transition:
 *   decree-backed compulsory adoption for a state-employed fringe (dress
 *   regulations, the 1873 Gregorian calendar, revised oaths and ceremonies),
 *   followed by accelerating voluntary and socially pressured adoption
 *   outward through commerce, schooling, and administration until conformity
 *   is self-sustaining and legal coercion recedes. Assumptions stated
 *   plainly: time indexing is t = year minus 1868, so t0 is the restoration
 *   moment and tn is 1912, the close of the Meiji emperor's reign; the
 *   metrics assess the mechanism as it actually operated across that
 *   interval, not the pre-Meiji order it displaced and not any alternative
 *   pathway another reading might endorse. KEY AGENTS (by structural
 *   relationship): meiji_oligarchy - agenda-setter (institutional/arbitrage),
 *   authors and paces the mandate and captures legitimacy, fiscal reach, and
 *   treaty credibility; mandated_state_personnel - the manufactured fringe,
 *   initial payer and later insider (organized/constrained), whose public
 *   conformity is the climb's exemplar surface; dispossessed_samurai -
 *   status-stripped old-order bearers (moderate/identity_locked), split
 *   between absorption into the new order and armed resistance;
 *   rural_custom_practitioners - late-exposure payers (powerless/trapped)
 *   reached last through tax, school, and conscription; urban_adoption_elites
 *   - voluntary early adopters and beneficiaries (organized/mobile) who
 *   steepen the prestige gradient; foreign_treaty_powers - external
 *   beneficiaries (powerful/arbitrage) whose recognition is the prize the
 *   cascade collects; christian_missionary_networks - excluded voice
 *   expecting a different content of convergence;
 *   historical_sociology_observers - analytical seat.
 *
 * KEY AGENTS:
 *   - meiji_oligarchy: agenda-setter (institutional/arbitrage) - authors and paces the edicts; collects legitimacy, administrative legibility, and treaty credibility
 *   - mandated_state_personnel: manufactured fringe, dual-positioned (organized/constrained) - absorbs the first compliance costs while converting adoption into salary, rank, and standing; their public performance is the climb's exemplar surface
 *   - dispossessed_samurai: old-order identity bearers (moderate/identity_locked) - stipend commutation and marker bans strike fused identity; response splits between enlistment into the new order and the 1877 Satsuma rising
 *   - rural_custom_practitioners: late-exposure bearers (powerless/trapped) - meet the standard through the tax collector, schoolteacher, and draft board; pay assimilation labor with thin compensation
 *   - urban_adoption_elites: voluntary early adopters (organized/mobile) - convert adoption into credit, profession, and marriage-market advantage; advertise the payoff
 *   - foreign_treaty_powers: external beneficiaries (powerful/arbitrage) - trade recognition and treaty revision for visible convergence
 *   - christian_missionary_networks: excluded voice (organized/constrained) - bet on conversion content and were routed around by state-aligned civic forms
 *   - historical_sociology_observers: analytical seat - code the cascade and hand the classification to later readers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_pathway_kernel__hybrid_cascade_reading, 0.66).
domain_priors:suppression_score(imposition_pathway_kernel__hybrid_cascade_reading, 0.38).
domain_priors:theater_ratio(imposition_pathway_kernel__hybrid_cascade_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_pathway_kernel__hybrid_cascade_reading, tangled_rope).
narrative_ontology:human_readable(imposition_pathway_kernel__hybrid_cascade_reading, "Hybrid Cascade Pathway: Mandated Fringe Seeding Organic Commitment Climb (Meiji Reading)").
narrative_ontology:topic_domain(imposition_pathway_kernel__hybrid_cascade_reading, "historical sociology / state formation / commitment systems").

domain_priors:requires_active_enforcement(imposition_pathway_kernel__hybrid_cascade_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_pathway_kernel__hybrid_cascade_reading, 'f1a5b834-2aea-4777-adc2-4dd1779ba0d3').
narrative_ontology:cs_kernel_codification('f1a5b834-2aea-4777-adc2-4dd1779ba0d3', distributed).
narrative_ontology:cs_authority_grounding('f1a5b834-2aea-4777-adc2-4dd1779ba0d3', expertise).
narrative_ontology:cs_reading_relation('f1a5b834-2aea-4777-adc2-4dd1779ba0d3', imposition_pathway_kernel__endogenous_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('f1a5b834-2aea-4777-adc2-4dd1779ba0d3', imposition_pathway_kernel__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('f1a5b834-2aea-4777-adc2-4dd1779ba0d3', foundational, manufactured_fringe_does_causal_work).
narrative_ontology:cs_axiom_status(manufactured_fringe_does_causal_work, holdable).
narrative_ontology:cs_axiom_grounding('f1a5b834-2aea-4777-adc2-4dd1779ba0d3', manufactured_fringe_does_causal_work, empirically_contingent).
narrative_ontology:cs_axiom('f1a5b834-2aea-4777-adc2-4dd1779ba0d3', foundational, override_initiates_climb_completes).
narrative_ontology:cs_axiom_status(override_initiates_climb_completes, holdable).
narrative_ontology:cs_axiom_grounding('f1a5b834-2aea-4777-adc2-4dd1779ba0d3', override_initiates_climb_completes, empirically_contingent).
narrative_ontology:cs_reference_frame('f1a5b834-2aea-4777-adc2-4dd1779ba0d3', hybrid_cascade_reference_sequence).
narrative_ontology:cs_drift_state('f1a5b834-2aea-4777-adc2-4dd1779ba0d3', comparative_microhistorical_era, gap(axiom_overriding, minor, false)).
narrative_ontology:cs_created_at('f1a5b834-2aea-4777-adc2-4dd1779ba0d3', '').
narrative_ontology:cs_kernel_id(imposition_pathway_kernel__hybrid_cascade_reading, imposition_pathway_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__hybrid_cascade_reading, meiji_oligarchy).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__hybrid_cascade_reading, urban_adoption_elites).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__hybrid_cascade_reading, foreign_treaty_powers).
narrative_ontology:constraint_victim(imposition_pathway_kernel__hybrid_cascade_reading, mandated_state_personnel).
narrative_ontology:constraint_victim(imposition_pathway_kernel__hybrid_cascade_reading, dispossessed_samurai).
narrative_ontology:constraint_victim(imposition_pathway_kernel__hybrid_cascade_reading, rural_custom_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__hybrid_cascade_reading, mandated_state_personnel).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues and paces the edicts that compel Western-form dress, the Gregorian calendar, and revised administrative and ritual forms among officials, officers, and teachers; decides which domains are mandated and which are left to drift. Collects the returns: administrative legibility, fiscal reach, and the treaty credibility that external recognition rewards. Exit from the arrangement is trivial - it wrote the rules and can re-price them at will.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, meiji_oligarchy, agenda_setter,
    institutional, generational, arbitrage, national).

% Bureaucrats, army and navy officers, police, and schoolteachers ordered to wear specified dress, keep the new calendar, perform revised ceremonies, and subscribe to loyalty rescripts. They absorb the first compliance costs - tailoring bills, disrupted household custom, ridicule from kin - while gaining salaries, promotion eligibility, and standing inside the new order. Leaving means resigning the post; staying means performing the standard in public view daily, which is precisely what makes them the visible exemplar class others calibrate against.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, mandated_state_personnel, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(imposition_pathway_kernel__hybrid_cascade_reading, mandated_state_personnel, beneficiary).

% Former domain warriors whose hereditary stipends were commuted to bonds, whose swords and topknots were proscribed, and whose sumptuary world was dismantled between 1871 and 1879. Some take commissions in the new conscript army or posts in the prefectures - entering the mandated class from above - while others read the same measures as dishonor and rise in armed protest, culminating in the 1877 Satsuma war. Their status identity was fused with the prohibited markers, so even where material paths exist, abandoning them feels like self-erasure; descendants largely assimilate within a generation.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, dispossessed_samurai, payer,
    moderate, biographical, identity_locked, regional).

% Village households who encounter the new standards last and indirectly - through the tax collector's calendar, the schoolteacher's rescript, the conscription board's registers, and returning sons in uniform. They pay in assimilation labor: renamed rites, re-timed festivals, children disciplined into standard speech and loyalty formulas. Compensation arrives thin and late; folk practice continues privately where surveillance is thin, and narrows as schooling and registration deepen.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, rural_custom_practitioners, payer,
    powerless, generational, trapped, regional).

% Merchants, students, journalists, and physicians in the treaty-port cities who adopt early and voluntarily, before or without compulsion. Adoption buys trade credit with foreign houses, positions in the new professions, and standing in the marriage market; their visible success advertises the standard's payoff and steepens the prestige gradient down which the climb travels.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, urban_adoption_elites, beneficiary,
    organized, biographical, mobile, national).

% The treaty-signing governments whose extraterritorial jurisdiction and capped tariffs were justified by Japan's purportedly backward legal order. They reward convergence - revised codes, western dress at court, calendar alignment - with negotiated treaty revision from 1894 onward, supplying the external prize the cascade was engineered to collect.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, foreign_treaty_powers, beneficiary,
    powerful, generational, arbitrage, global).

% Mission societies that read the westernizing decrees as an opening for mass conversion and invested in schools and hospitals accordingly. They find adoption channeled instead into state-aligned civic forms, watch the 1890 Imperial Rescript place Shinto-grounded morality at the center of schooling, and end the period confined to tolerated niches. They would have pressed for a different content of convergence and were never seated where that content was decided.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, christian_missionary_networks, excluded,
    organized, generational, constrained, national).

% Reconstruct the cascade from ministry files, prefectural reports, missionary correspondence, and household inventories; code the episode against other modernization sequences; publish the classifications later readers inherit. Their stake is interpretive standing rather than any flow the arrangement distributes.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, historical_sociology_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imposition_pathway_kernel__hybrid_cascade_reading, meiji_oligarchy).
narrative_ontology:fixing_cost_class(imposition_pathway_kernel__hybrid_cascade_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves rapid commitment-standard unification: converts an administratively heterogeneous polity's temporal, presentational, and procedural norms into one legible standard quickly enough to satisfy treaty partners and central administration, by concentrating mandatory adoption on a small state-employed class whose visible conformity re-prices adoption for everyone else.
% TRANSFER_FUNCTION: Moves adoption costs onto salaried state personnel first (compelled dress, calendar, ceremony) and assimilation costs onto customary practitioners last; moves legitimacy, administrative legibility, and treaty credibility to the center; and moves status and commercial advantage to early voluntary adopters.
% ABSENT_VOICES: Customary practitioners outside elite discourse - village ritual specialists, elderly custodians of local custom, rural women - had no seat when the edicts were framed; their objection, that unification priced their lifeworld as residue, surfaces only in later folklore records. Christian mission networks expected consultation and were sidelined after 1890. Both voices exist in the archive at the margins, not in the decision rooms.
% DISAPPEARANCE_RATIONALE: Without the manufactured fringe, displacement proceeds at persuasion speed or requires permanent universal enforcement; the Meiji settlement's decade-scale unification, the treaty-revision timeline, the careers built on early adoption, and the template later modernizing states copied all depend on the fringe mechanism. Remove it overnight and administrative uniformity, external recognition strategy, and the status economy of adoption all rearrange.
% FOUNDING_PROBLEM: Post-Restoration Japan faced a compound legitimacy crisis: a revolutionary government needed rapid, visible convergence on recognized civilized standards to renegotiate the unequal treaties and bind a fragmented polity, before internal reaction or external predation undid the restoration.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: diplomatic historians document treaty-revision deadlines driving the reform program; comparative sociologists attest both the founding problem's resolution (revision completed 1894-1911) and the mechanism's afterlife as a portable template in later modernization episodes (Kemalist Turkey, Soviet nationality policy). No insider party disputes the problem's existence, but its resolved status rests on later scholarship rather than on the oligarchy, which had standing incentives to depict the threats as permanent.
narrative_ontology:disappearance_verdict(imposition_pathway_kernel__hybrid_cascade_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_pathway_kernel__hybrid_cascade_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_pathway_kernel__hybrid_cascade_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(imposition_pathway_kernel__hybrid_cascade_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_pathway_kernel__hybrid_cascade_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_pathway_kernel__hybrid_cascade_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imposition_pathway_kernel__hybrid_cascade_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imposition_pathway_kernel__hybrid_cascade_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.66 and rising across the interval because the mechanism's cost structure migrates: it begins concentrated on one salaried class (real but bounded) and ends diffuse - assimilation labor priced onto customary practitioners and holdouts whose alternatives have narrowed as the climb completed. Suppression_requirement is authored FALLING (0.72 to 0.38) because that trajectory is this reading's signature: the legal override does heavy initial work (dress and calendar edicts, sword prohibition, stipend commutation backed by inspection and, when resisted, force), and as the manufactured fringe normalizes the standard, formal coercion recedes and conformity reproduces itself through schools, registries, and social pricing. Theater_ratio rises gently (0.15 to 0.30): the mechanism is functionally load-bearing throughout, but once unification is secured, ceremonial display of the achieved standard (court dress, rescript ritual, anniversary pageantry) occupies a growing share of activity. Accessibility_collapse at 0.55 reflects partial closure: private and peripheral pockets of the old commitments persist, but public alternatives are largely priced out by tn. Resistance at 0.30 reflects the arc from armed and riotous early opposition (haircut-edict riots, Satsuma 1877) to quiet private retention. The claim and the metrics are authored independently: claimed_type tangled_rope states what I believe is structurally true - a genuine coordination good (rapid standardization delivering administrative unity and treaty credibility) fused with asymmetric extraction (customary bearers and status-stripped samurai pay; the center and early adopters gain) sustained by active enforcement during the mandate phase. The measurement series run on one single shared grid (t = 0, 8, 15, 22, 29, 36, 44) across all tracked metrics; no oscillation is asserted - the drift is monotone, driven by the cascade's staged structure rather than any intermittent-reinforcement cycle.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the agenda-setter seat the cascade is a nation-saving bargain that economized on coercion - pay one class to normalize what would otherwise require policing everyone. From the mandated-fringe seat it is a double position: first payer and then insider, with gatekeeping rents over later adopters. From the samurai and rural seats the same sequence is dispossession - identity markers outlawed, festivals retimed, livelihoods repriced - arriving with no compensating channel. Among same-nominal-standing actors the differentiation is constraint-specific: domain samurai and appointed officials occupied adjacent status strata in 1870, but exit diverged sharply - the official's exit is career-constrained (resignation costs the post), the samurai's identity-fused (abandoning the markers costs the self) - which is why equal-rank actors sit at opposed directionalities. Foreign treaty powers experience the constraint from outside as a convergence payoff, not a burden at all.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation. The oligarchy sits at the beneficiary pole (collects legitimacy and capacity, controls pacing, arbitrage-grade exit, d near 0.05); foreign treaty powers benefit indirectly and sit low (recognition-for-convergence, arbitrage exit, d near 0.15); urban adoption elites benefit from the prestige gradient they help steepen (d near 0.25). Rural customary practitioners sit near the full-target pole (trapped, taxed into the standard, d near 0.9); dispossessed samurai near it as well (identity-locked targets, d near 0.85). The mandated fringe is the deliberately dual case: listed among victims for its first-borne compliance costs and carrying a beneficiary secondary role for salary, rank, and gatekeeping returns - the derivation reads that combination as an intermediate directionality rather than a pure target. No directionality_overrides are authored: the one seat needing correction from a naive victim-reading is dual-listed precisely so the structural data yields the mixed d, and the schema keys overrides by power atom rather than by agent, so an override at the organized atom would contaminate the clean beneficiary seats sharing that atom (urban adoption elites). Suppression is authored as a raw structural property and is not scaled; only extractiveness is scaled by directionality and scope in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - rapid unification to survive the treaty regime - is dead: revision completed by 1911 and unification held. Yet the arrangement persists, not as administration but as a portable template that later modernizing states invoke. The founding_problem_status x disappearance_verdict mismatch (dead x world_rearranges) is therefore authored deliberately and should flag template persistence rather than being smoothed away. Classification guards both errors: reading the cascade as pure extraction ignores the coordination good actually delivered at falling coercive cost; reading it as pure coordination ignores that the standardization was financed by uncompensated bearers - the mandated class first, customary practitioners last. The tangled_rope verdict holds for the Meiji instantiation; the snare-drift risk attaches to template reuse, where later adopters copy the coercion choreography without the founding emergency, and the extraction half detaches from the coordination half. The omega on generalizability routes exactly that question forward.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fringe_manufacture_vs_visibility,
    'Is the mandated class a state-manufactured climb vector with causal work of its own, or a made-visible segment of a pre-existing diffusion that the decree merely compressed and illuminated?',
    'Counterfactual adoption-rate reconstruction: compare pre-decree voluntary adoption trajectories among unmandated strata (merchant diaries, treaty-port records) against post-decree acceleration curves; a discontinuity attributable to exemplar exposure rather than price or information effects supports manufacture.',
    'If visibility-only, this reading collapses toward the endogenous sibling and epsilon re-references a pure diffusion arrangement with lower suppression; if manufactured, the override stage carries irreducible causal weight and this reading stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fringe_manufacture_vs_visibility, empirical, 'Whether the artificial fringe was manufactured by the decree or merely rendered visible by it - the core contested element against the endogenous sibling reading.').

omega_variable(
    override_stage_necessity,
    'Does the legal override stage accomplish displacement that organic climb alone could not achieve at acceptable timescale or cost, or is it substitutable by incentive design?',
    'Structured comparison of modernization episodes with and without personnel mandates (Kemalist Turkey, Chakri-era Siam, Qing self-strengthening), controlling for state capacity, external pressure, and starting heterogeneity.',
    'If override is substitutable, the mechanism reduces to a fast rope and the exogenous sibling''s separate-cell demand loses its object; if irreplaceable, the exogenous sibling gains a distinct mechanism and this hybrid reading becomes a special case bridging both cells.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(override_stage_necessity, empirical, 'Necessity of the override stage - the axis on which the exogenous sibling reading challenges this one.').

omega_variable(
    fringe_net_position_ambiguity,
    'Were mandated personnel net extraction targets who paid first, or net subsidized instruments whose career and status returns exceeded compliance costs?',
    'Welfare tracing of the mandated class against comparable unmandated elites across the interval: salary trajectories, promotion rates, pension outcomes, marriage-market positioning.',
    'Sets that seat''s directionality and hence its computed type: a net-subsidized fringe weakens the extraction half and pulls the classification toward rope; a net-losing fringe sharpens extraction and pulls toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fringe_net_position_ambiguity, conceptual, 'Ambiguity in the dual-positioned fringe seat''s net ledger, which the beneficiary-plus-victim dual declaration leaves to the engine to weigh.').

omega_variable(
    cascade_generalizability,
    'Is the hybrid cascade a portable mechanism of commitment displacement or an artifact of Meiji-specific boundary conditions (unified bureaucratic command, weak civil society, acute external legitimacy pressure)?',
    'Identify which boundary conditions recur across modernization episodes and test template outcomes where they are absent; track whether later template adopters reproduce the coordination half or import only the coercion choreography.',
    'If portable, the constraint binds future modernization planning and the template-persistence warning carries forward; if artifactual, the classification describes a closed episode and template-inheritance warnings lapse.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cascade_generalizability, empirical, 'Portability of the mechanism beyond its founding episode - the question that governs whether the founding-problem mismatch flags living template risk.').

omega_variable(
    samurai_identity_lock_mechanism,
    'How much of the dispossessed samurai seat''s immobility was structural (legal bans, stipend commutation) and how much internalized (honor-code fusion making abandonment unthinkable)?',
    'Post-liberalization behavior: after sword prohibitions lapsed and stipends converted to bonds, compare marker-retention rates among samurai-descended lineages against material-cost predictions; persistence beyond material rationality indicates internalized fusion.',
    'If substantially internalized, the seat carries suppression with it past the structural barrier removal and its effective extraction exceeds the structural measure; the seat''s exit classification shifts from trapped-toward-identity_locked accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(samurai_identity_lock_mechanism, empirical, 'Structural versus internalized composition of the old-order seat''s suppression, informing the interpersonal-suppression ambiguity handling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_pathway_kernel__hybrid_cascade_reading, 0, 44).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t0, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(impo_tr_t8, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 8, 0.17).
narrative_ontology:measurement(impo_tr_t15, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement(impo_tr_t22, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 22, 0.23).
narrative_ontology:measurement(impo_tr_t29, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 29, 0.26).
narrative_ontology:measurement(impo_tr_t36, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 36, 0.28).
narrative_ontology:measurement(impo_tr_t44, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 44, 0.3).

% Extraction over time
narrative_ontology:measurement(impo_be_t0, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(impo_be_t8, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 8, 0.53).
narrative_ontology:measurement(impo_be_t15, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 15, 0.56).
narrative_ontology:measurement(impo_be_t22, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 22, 0.59).
narrative_ontology:measurement(impo_be_t29, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 29, 0.61).
narrative_ontology:measurement(impo_be_t36, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 36, 0.64).
narrative_ontology:measurement(impo_be_t44, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 44, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t0, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(impo_su_t8, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 8, 0.68).
narrative_ontology:measurement(impo_su_t15, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement(impo_su_t22, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 22, 0.54).
narrative_ontology:measurement(impo_su_t29, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 29, 0.47).
narrative_ontology:measurement(impo_su_t36, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 36, 0.42).
narrative_ontology:measurement(impo_su_t44, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 44, 0.38).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=44
narrative_ontology:measurement(impo_grid_01, imposition_pathway_kernel__hybrid_cascade_reading, accessibility_collapse(class), 0, 0.5).
narrative_ontology:measurement_basis(impo_grid_01, observed).
narrative_ontology:measurement(impo_grid_02, imposition_pathway_kernel__hybrid_cascade_reading, accessibility_collapse(class), 44, 0.7).
narrative_ontology:measurement_basis(impo_grid_02, observed).
narrative_ontology:measurement(impo_grid_03, imposition_pathway_kernel__hybrid_cascade_reading, accessibility_collapse(individual), 0, 0.75).
narrative_ontology:measurement_basis(impo_grid_03, observed).
narrative_ontology:measurement(impo_grid_04, imposition_pathway_kernel__hybrid_cascade_reading, accessibility_collapse(individual), 44, 0.6).
narrative_ontology:measurement_basis(impo_grid_04, observed).
narrative_ontology:measurement(impo_grid_05, imposition_pathway_kernel__hybrid_cascade_reading, accessibility_collapse(organizational), 0, 0.8).
narrative_ontology:measurement_basis(impo_grid_05, observed).
narrative_ontology:measurement(impo_grid_06, imposition_pathway_kernel__hybrid_cascade_reading, accessibility_collapse(organizational), 44, 0.85).
narrative_ontology:measurement_basis(impo_grid_06, observed).
narrative_ontology:measurement(impo_grid_07, imposition_pathway_kernel__hybrid_cascade_reading, accessibility_collapse(structural), 0, 0.15).
narrative_ontology:measurement_basis(impo_grid_07, observed).
narrative_ontology:measurement(impo_grid_08, imposition_pathway_kernel__hybrid_cascade_reading, accessibility_collapse(structural), 44, 0.55).
narrative_ontology:measurement_basis(impo_grid_08, observed).
narrative_ontology:measurement(impo_grid_09, imposition_pathway_kernel__hybrid_cascade_reading, resistance(class), 0, 0.65).
narrative_ontology:measurement_basis(impo_grid_09, observed).
narrative_ontology:measurement(impo_grid_10, imposition_pathway_kernel__hybrid_cascade_reading, resistance(class), 44, 0.1).
narrative_ontology:measurement_basis(impo_grid_10, observed).
narrative_ontology:measurement(impo_grid_11, imposition_pathway_kernel__hybrid_cascade_reading, resistance(individual), 0, 0.3).
narrative_ontology:measurement_basis(impo_grid_11, observed).
narrative_ontology:measurement(impo_grid_12, imposition_pathway_kernel__hybrid_cascade_reading, resistance(individual), 44, 0.25).
narrative_ontology:measurement_basis(impo_grid_12, observed).
narrative_ontology:measurement(impo_grid_13, imposition_pathway_kernel__hybrid_cascade_reading, resistance(organizational), 0, 0.25).
narrative_ontology:measurement_basis(impo_grid_13, observed).
narrative_ontology:measurement(impo_grid_14, imposition_pathway_kernel__hybrid_cascade_reading, resistance(organizational), 44, 0.15).
narrative_ontology:measurement_basis(impo_grid_14, observed).
narrative_ontology:measurement(impo_grid_15, imposition_pathway_kernel__hybrid_cascade_reading, resistance(structural), 0, 0.4).
narrative_ontology:measurement_basis(impo_grid_15, observed).
narrative_ontology:measurement(impo_grid_16, imposition_pathway_kernel__hybrid_cascade_reading, resistance(structural), 44, 0.35).
narrative_ontology:measurement_basis(impo_grid_16, observed).
narrative_ontology:measurement(impo_grid_17, imposition_pathway_kernel__hybrid_cascade_reading, stakes_inflation(class), 0, 0.6).
narrative_ontology:measurement_basis(impo_grid_17, observed).
narrative_ontology:measurement(impo_grid_18, imposition_pathway_kernel__hybrid_cascade_reading, stakes_inflation(class), 44, 0.35).
narrative_ontology:measurement_basis(impo_grid_18, observed).
narrative_ontology:measurement(impo_grid_19, imposition_pathway_kernel__hybrid_cascade_reading, stakes_inflation(individual), 0, 0.75).
narrative_ontology:measurement_basis(impo_grid_19, observed).
narrative_ontology:measurement(impo_grid_20, imposition_pathway_kernel__hybrid_cascade_reading, stakes_inflation(individual), 44, 0.45).
narrative_ontology:measurement_basis(impo_grid_20, observed).
narrative_ontology:measurement(impo_grid_21, imposition_pathway_kernel__hybrid_cascade_reading, stakes_inflation(organizational), 0, 0.7).
narrative_ontology:measurement_basis(impo_grid_21, observed).
narrative_ontology:measurement(impo_grid_22, imposition_pathway_kernel__hybrid_cascade_reading, stakes_inflation(organizational), 44, 0.6).
narrative_ontology:measurement_basis(impo_grid_22, observed).
narrative_ontology:measurement(impo_grid_23, imposition_pathway_kernel__hybrid_cascade_reading, stakes_inflation(structural), 0, 0.2).
narrative_ontology:measurement_basis(impo_grid_23, observed).
narrative_ontology:measurement(impo_grid_24, imposition_pathway_kernel__hybrid_cascade_reading, stakes_inflation(structural), 44, 0.4).
narrative_ontology:measurement_basis(impo_grid_24, observed).
narrative_ontology:measurement(impo_grid_25, imposition_pathway_kernel__hybrid_cascade_reading, suppression(class), 0, 0.7).
narrative_ontology:measurement_basis(impo_grid_25, observed).
narrative_ontology:measurement(impo_grid_26, imposition_pathway_kernel__hybrid_cascade_reading, suppression(class), 44, 0.2).
narrative_ontology:measurement_basis(impo_grid_26, observed).
narrative_ontology:measurement(impo_grid_27, imposition_pathway_kernel__hybrid_cascade_reading, suppression(individual), 0, 0.8).
narrative_ontology:measurement_basis(impo_grid_27, observed).
narrative_ontology:measurement(impo_grid_28, imposition_pathway_kernel__hybrid_cascade_reading, suppression(individual), 44, 0.35).
narrative_ontology:measurement_basis(impo_grid_28, observed).
narrative_ontology:measurement(impo_grid_29, imposition_pathway_kernel__hybrid_cascade_reading, suppression(organizational), 0, 0.75).
narrative_ontology:measurement_basis(impo_grid_29, observed).
narrative_ontology:measurement(impo_grid_30, imposition_pathway_kernel__hybrid_cascade_reading, suppression(organizational), 44, 0.45).
narrative_ontology:measurement_basis(impo_grid_30, observed).
narrative_ontology:measurement(impo_grid_31, imposition_pathway_kernel__hybrid_cascade_reading, suppression(structural), 0, 0.35).
narrative_ontology:measurement_basis(impo_grid_31, observed).
narrative_ontology:measurement(impo_grid_32, imposition_pathway_kernel__hybrid_cascade_reading, suppression(structural), 44, 0.5).
narrative_ontology:measurement_basis(impo_grid_32, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_pathway_kernel__hybrid_cascade_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(imposition_pathway_kernel__hybrid_cascade_reading, imposition_pathway_kernel__endogenous_climb_reading).
narrative_ontology:affects_constraint(imposition_pathway_kernel__hybrid_cascade_reading, imposition_pathway_kernel__exogenous_override_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'top-down imposition of new commitments' decomposes into three structurally distinct constraints per the epsilon-invariance principle, forming the imposition_pathway_kernel family. The endogenous reading (pure climb, invisible fringe stages; epsilon keyed to a diffusion arrangement) is the established upstream baseline claim. The exogenous override reading (capacity-based displacement bypassing the fringe pathway; epsilon keyed to a pure-enforcement arrangement) is the most contested downstream proposal. This hybrid reading sits between them, citing both: epsilon keyed to the mandate-and-climb arrangement authored here. Each file carries its own stable epsilon, beneficiary/victim structure, and classification, linked through network.affects_constraints in all three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
