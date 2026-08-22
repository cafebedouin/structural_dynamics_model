% ============================================================================
% CONSTRAINT STORY: honor_settlement_legitimacy__composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_settlement_legitimacy_composite_reading, []).

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
 *   constraint_id: honor_settlement_legitimacy__composite_reading
 *   human_readable: Honor-Legitimacy Settlement Regime (Composite Reading: Overdetermined Decline)
 *   domain: legal/social/cultural
 *
 * SUMMARY:
 *   Dueling as a status-settlement mechanism declined across Western Europe
 *   and North America between the 16th and 19th centuries through multiple
 *   reinforcing causal pathways: cultural delegitimation (honor became
 *   unthinkable as a rational basis for violence; Enlightenment redescription
 *   of honor as barbaric); legal prohibition and enforcement machinery
 *   (states imposed capital punishment for dueling and prosecuted it
 *   aggressively); economic transformation (rising bourgeoisie had status
 *   capital in wealth, not honor, making dueling unwinnable by their logic);
 *   institutional absorption (legal courts monopolized dispute settlement and
 *   delegitimized private remedies); religious delegitimation (churches
 *   reclassified honor-killing as mortal sin). This reading frames the
 *   decline as **composite and overdetermined**: no single pathway is
 *   sufficient; all pathways were independent material/institutional changes
 *   that converged on a shared outcome. Cultural unthinkability (contraction)
 *   is the primary terminal driver (alternatives became literally
 *   unimaginable within educated society), but it is causally intertwined
 *   with material suppression that would independently have undermined
 *   dueling even without cultural collapse. The constraint story describes
 *   the standing arrangement—the honor-settlement regime as it persisted in
 *   early modernity—assessed by this reading's own lights: high
 *   extractiveness because the regime extracted loyalty and life commitment
 *   from practitioners; high suppression because escaping the regime required
 *   cognitive/social/institutional impossibility, not mere legal barrier; low
 *   theater in the early period (the practice was functionally real) and
 *   rising theater as the practice persisted only through residual
 *   performance after legitimacy eroded.
 *
 * KEY AGENTS:
 *   - gentry_with_status_capital: Practitioners of honor-settlement who depended on status-through-combat; held identity-locked commitment to dueling logic; bore the primary extraction (life, wealth, time) and faced rising suppression.
 *   - emerging_bourgeois_legal_system: Institutional beneficiary; state monopoly on dispute settlement; prosecuted dueling as crime; delegitimated private remedies; extracted legitimacy gain by absorbing settlement function.
 *   - state_monopoly_authority: Beneficiary and agenda-setter; enforced prohibition; built legal infrastructure that made dueling illegal and socially impossible; collected judicial authority as the only legitimate settlement mechanism.
 *   - religious_institutional_authority: Secondary beneficiary; delegitimated honor-killing as sin; reinforced cultural unthinkability through moral redescription.
 *   - merchant_and_professional_classes: Secondary beneficiaries; rose in social prominence with wealth-based status; had no interest in honor-settlement; contributed to cultural shift toward rational dispute resolution.
 *   - observers_enlightenment_intellectuals: Analytical seats; produced cultural redescription of honor as barbaric; framed dueling as irrational; provided legitimacy rationale for legal suppression.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_settlement_legitimacy__composite_reading, 0.62).
domain_priors:suppression_score(honor_settlement_legitimacy__composite_reading, 0.58).
domain_priors:theater_ratio(honor_settlement_legitimacy__composite_reading, 0.21).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, theater_ratio, 0.21).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, accessibility_collapse, 0.91).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, resistance, 0.34).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_settlement_legitimacy__composite_reading, tangled_rope).
narrative_ontology:human_readable(honor_settlement_legitimacy__composite_reading, "Honor-Legitimacy Settlement Regime (Composite Reading: Overdetermined Decline)").
narrative_ontology:topic_domain(honor_settlement_legitimacy__composite_reading, "legal/social/cultural").

domain_priors:requires_active_enforcement(honor_settlement_legitimacy__composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_settlement_legitimacy__composite_reading, '43bf2996-1d28-4914-9303-44268557940f').
narrative_ontology:cs_kernel_codification('43bf2996-1d28-4914-9303-44268557940f', distributed).
narrative_ontology:cs_authority_grounding('43bf2996-1d28-4914-9303-44268557940f', extraction).
narrative_ontology:cs_reading_relation('43bf2996-1d28-4914-9303-44268557940f', honor_settlement_legitimacy__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('43bf2996-1d28-4914-9303-44268557940f', honor_settlement_legitimacy__drop_reading, coexists_with).
narrative_ontology:cs_axiom('43bf2996-1d28-4914-9303-44268557940f', foundational, multiple_causal_pathways_overdetermined).
narrative_ontology:cs_axiom_status(multiple_causal_pathways_overdetermined, holdable).
narrative_ontology:cs_axiom_grounding('43bf2996-1d28-4914-9303-44268557940f', multiple_causal_pathways_overdetermined, empirically_contingent).
narrative_ontology:cs_axiom('43bf2996-1d28-4914-9303-44268557940f', foundational, cultural_contraction_primary_terminal_driver).
narrative_ontology:cs_axiom_status(cultural_contraction_primary_terminal_driver, holdable).
narrative_ontology:cs_axiom_grounding('43bf2996-1d28-4914-9303-44268557940f', cultural_contraction_primary_terminal_driver, empirically_contingent).
narrative_ontology:cs_axiom('43bf2996-1d28-4914-9303-44268557940f', secondary, institutional_material_suppression_mutually_reinforcing).
narrative_ontology:cs_axiom_status(institutional_material_suppression_mutually_reinforcing, holdable).
narrative_ontology:cs_axiom_grounding('43bf2996-1d28-4914-9303-44268557940f', institutional_material_suppression_mutually_reinforcing, empirically_contingent).
narrative_ontology:cs_reference_frame('43bf2996-1d28-4914-9303-44268557940f', honor_settlement_as_legitimate_dispute_mechanism).
narrative_ontology:cs_drift_state('43bf2996-1d28-4914-9303-44268557940f', post_enlightenment_legal_consolidation_era, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('43bf2996-1d28-4914-9303-44268557940f', '2026-06-12T14:22:00Z').
narrative_ontology:cs_kernel_id(honor_settlement_legitimacy__composite_reading, honor_settlement_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__composite_reading, emerging_bourgeois_legal_system).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__composite_reading, state_monopoly_authority).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__composite_reading, honor_culture_adherents).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__composite_reading, gentry_with_status_capital).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__composite_reading, religious_institutional_authority).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__composite_reading, merchant_and_professional_classes).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__composite_reading, residual_fringe_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The practitioners and defenders of honor-settlement. Early modernity (t=0): dueling is a costly but rational strategy within status-competition among gentlemen; legal system does not yet monopolize dispute settlement; honor is culturally legitimate. As interval progresses (t=0 to t=4): dueling becomes legally prohibited, culturally delegitimated, economically pointless (wealth-based status replaces combat-based rank), religiously condemned, institutionally isolated. Gentry face rising suppression across all registers: legal penalties increase, cultural status of dueling plummets, merchant/professional classes (who do not duel) rise in social prominence, younger cohorts are socialized away from honor-settlement logic. Exit appears available (refuse to duel, accept legal settlement) but is identity-locked: for practitioners whose self-concept is constituted through honor and dueling prowess, exit means status death and self-dissolution. By t=4, surviving practitioners are almost entirely identity-locked (older cohorts, cultural holdouts); younger generations have escaped through cognitive/social impossibility rather than deliberate choice.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, gentry_with_status_capital, payer,
    organized, biographical, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(honor_settlement_legitimacy__composite_reading, gentry_with_status_capital, excluded).

% The beneficiary and administrator of the constraint's decline. The state builds legal authority over dispute settlement: legal courts absorb the settlement function, capital punishment is imposed for dueling, enforcement machinery is operationalized. Early modernity: state enforcement is present but spotty and culturally contested (dueling persists despite illegality because cultural legitimacy is strong). By t=4: state enforcement has hardened, cultural backup has eroded, legal monopoly is consolidated and uncontested. The state benefits by concentrating settlement authority, eliminating private remedies, and expanding its monopoly over legitimate violence. Exit options are arbitrage-grade: the state can extend settlement monopoly into other domains (blood feuds, private warfare, vendetta resolution) or relax enforcement if politically advantageous; it has institutional flexibility because it is not bound by commitment to dueling's survival.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, state_monopoly_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% The institutional and epistemic beneficiary of the shift from honor-settlement to legal settlement. As merchants and professionals rise in social prominence, legal dispute resolution becomes attractive (predictable, wealth-compatible, non-lethal, compatible with contractual economy). Legal system benefits by absorbing settlement authority, gaining legitimacy and jurisdiction, expanding its scope to regulate commercial disputes (which the honor-code could not handle). Early modernity: legal system competes with honor-settlement; lawyers and judges are building a rival authority structure. By t=4: legal system has won; honor-settlement is legal anachronism; legal dispute resolution is culturally normal. Exit options are mobile: the legal system can adapt to new dispute types, incorporate new stakeholders, evolve procedural norms; it is not bound to any particular settlement mechanism.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, emerging_bourgeois_legal_system, beneficiary,
    institutional, generational, mobile, national).

% Secondary beneficiary; reinforces cultural delegitimation of dueling. Religious institutions (Catholic and Protestant churches) reclassify honor-killing as mortal sin, deny sacraments to duelers, excommunicate participants. This reinforces cultural redescription of honor as barbaric and adds moral authority to legal prohibition. Early modernity: religious opposition to dueling is present but selective (some clerics are duelers themselves; religious authority is contested). By t=4: religious opposition is consolidated and culturally influential (younger cohorts are socialized away from dueling through religious education, moral frameworks). Exit options are constrained: religious institutions are bound to doctrinal positions on sin and violence; they can interpret doctrine flexibly but cannot simply abandon opposition to dueling without internal contradiction.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, religious_institutional_authority, beneficiary,
    institutional, generational, constrained, continental).

% Indirect beneficiaries of dueling's decline. Merchants and professionals rise in social prominence through wealth and expertise rather than combat prowess. They have no stake in honor-settlement (they cannot win duels through wealth, cannot compete in honor-status with military gentry). They benefit from legal settlement machinery as a coordination mechanism for commercial disputes. Early modernity: merchants are subordinate to gentry in status hierarchy; dueling is not available to them (some laws restrict dueling to nobility). By t=4: merchants are dominant in status hierarchy; legal settlement is normal and compatible with commercial economy; honor-settlement is residual. Exit options are mobile: merchant classes can shift their status-competition mechanisms, adopt new dispute-settlement norms, engage with legal system on favorable terms.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, merchant_and_professional_classes, beneficiary,
    powerful, generational, mobile, national).

% Analytical seat; produces cultural redescription of dueling and honor. Enlightenment thinkers frame honor as barbaric, dueling as irrational, legal settlement as rational and civilized. This redescription provides intellectual legitimacy for legal prohibition and cultural delegitimation. Early modernity: Enlightenment criticism of dueling emerges but is contested (honor culture has its own intellectual defenders). By t=4: Enlightenment framing is culturally dominant (educated classes view honor-settlement as primitive). The intellectuals neither directly pay nor benefit (they are not practitioners); they produce the cultural framework that underpins both legal prohibition and gentry practitioners' own self-doubt.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, enlightenment_intellectuals, observer,
    analytical, generational, analytical, continental).

% By t=4, the remaining practitioners are almost entirely identity-locked, older cohorts (those socialized in honor-culture before cultural collapse), or isolated subcultures. They continue dueling despite complete delegitimation, legal prohibition, social ostracism, and religious condemnation. Exit is impossible without self-dissolution. The practice persists only through cultural isolation and identity-fusion. This group is distinct from gentry_with_status_capital in being powerless at t=4 (no longer commanding institutional authority or social prominence); they are holdouts rather than practitioners-in-power.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, residual_fringe_practitioners, payer,
    powerless, biographical, identity_locked, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_settlement_legitimacy__composite_reading, state_monopoly_authority).
narrative_ontology:fixing_cost_class(honor_settlement_legitimacy__composite_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Honor-settlement provided a mechanism for resolving disputes between gentlemen without invoking state authority or legal courts, while preserving the status and honor of both parties. The mechanism worked through institutionalized ritual (the duel) that allowed combatants to establish relative status through combat, then terminate the dispute and restore social relationships. This solved a genuine coordination problem in early modernity: legal courts were slow, expensive, inaccessible to most gentry, did not recognize status hierarchies, could not enforce outcomes against powerful actors, and did not provide the public ritual that restored honor after offense. Honor-settlement was faster, cheaper, accessible on demand, publicly performed, and culturally recognizable.
% TRANSFER_FUNCTION: Honor-settlement moved time, wealth, and life from practitioners to status competitors (the victor gained status, the loser lost status or lost his life). It also moved cultural authority from state authority to honor-community authority (the community that recognized honor settlements, not courts, defined the legitimacy of dispute outcomes). The mechanism was fundamentally about status transfer through combat; it worked by concentrating the cost (physical danger, death risk) on the participants and the benefit (status confirmation, honor restoration) on the survivor.
% ABSENT_VOICES: Merchants and professionals who did not practice dueling (either because they were excluded by law/custom or because dueling did not serve their status-competition mechanisms) would have objected to the honor-settlement regime if their voices had been heard. They would have argued that honor-settlement was irrational, wasteful, incompatible with commercial activity, and that legal settlement was superior. Lower classes would have objected that honor-settlement was available only to the elite and preserved gentry privilege by allowing them to settle disputes outside legal courts. Women would have objected that honor-settlement was an exclusively male mechanism and that their disputes were not recognized as amenable to honor-settlement. Religious reformers would have objected that dueling was sinful and that religious authority, not honor-community authority, should govern disputes.
% DISAPPEARANCE_RATIONALE: If honor-settlement had completely and permanently disappeared without the material/institutional/cultural alternatives that actually emerged, the world would rearrange itself toward different dispute-settlement mechanisms. Gentry would have been forced to rely on legal courts (which would have expanded capacity and prestige to absorb the demand). Informal arbitration and mediation would have developed to fill the gap (community elders, guild arbiters, ecclesiastical courts). The status-competition mechanisms within the gentry would have reorganized around non-lethal forms of competition (wealth, educational credentials, bureaucratic position, patronage networks). The state legal system would have faced demand pressure to become more accessible and responsive to gentry disputes. The disappearance is not natural—it required active cultural work (delegitimation), institutional change (legal monopoly), and material transformation (status redistribution toward wealth). The arrangement matters because its absence would force reorganization.
% FOUNDING_PROBLEM: Early modernity had a genuine dispute-settlement problem: gentry-to-gentry conflicts (insults, property disputes, romantic rivalries) could not be reliably or honorably settled through legal courts that were slow, expensive, inaccessible, and did not recognize the status hierarchies the disputes embodied. The legal system also could not enforce outcomes against powerful gentry with independent resources and armed retainers. Honor-settlement solved this by providing an immediate, public, ritualized mechanism for resolving disputes and restoring social relationships while respecting status hierarchies.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is corroborated by historical analysis from scholars OUTSIDE the benefiting parties (gentry, honor-community): legal historians document the slow/expensive/inaccessible character of early-modern courts; specialists in gentry culture document the centrality of honor-settlement to status competition; social historians document the gradual expansion and professionalization of legal systems that made state settlement faster and more accessible. By the 18th-19th centuries, independent observers (legal reformers, Enlightenment critics, merchants with commercial dispute needs) attest that the founding problem is SOLVED: legal courts are now faster, cheaper, accessible, and capable of enforcing outcomes. The founding problem is dead. The constraint persists despite the founding problem's resolution—this is the mandatrophy signal.
narrative_ontology:disappearance_verdict(honor_settlement_legitimacy__composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_settlement_legitimacy__composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_settlement_legitimacy__composite_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(honor_settlement_legitimacy__composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_settlement_legitimacy__composite_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_settlement_legitimacy__composite_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_settlement_legitimacy__composite_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_settlement_legitimacy__composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.48 (early modern: dueling was economically rational for gentry status competition) to 0.62 (composite peak: the regime persists only through cultural/institutional coercion after material incentives evaporate). The measurement series shows an inverted-U trajectory for extractiveness and theater, indicating the transition from functional practice to vestigial performance. Suppression begins near zero (early modernity: dueling was legal, culturally endorsed, organizationally enabled) and rises to 0.58 (late modern: dueling is illegal, criminally prosecuted, culturally unthinkable, economically pointless, religiously damned). The coercion grid reveals the composite mechanism: structural-level suppression (legal prohibition, institutional monopoly) rises from 0.08 to 0.58; organizational suppression (gentry status hierarchy reorganization around wealth, professionalization replacing combat-based rank) rises from 0.12 to 0.61; individual-level suppression (identity-lock erosion as practitioners aged without replacement, cognitive impossibility for younger cohorts) rises from 0.06 to 0.52. Resistance collapses at all four levels as cultural unthinkability combines with legal/material suppression: structural resistance (rival settling mechanisms available) falls from 0.68 to 0.18; organizational resistance (honor culture institutions) falls from 0.74 to 0.22; class resistance (gentry as a class defending dueling norms) falls from 0.61 to 0.31; individual resistance (practitioners' personal commitment to the practice) falls from 0.72 to 0.28. Theater rises from 0.08 (functional solution to status disputes) to 0.21 (the practice persists only as ritualized status-play after functional purpose evaporates).
 *
 * PERSPECTIVAL GAP:
 *   The gentry-practitioner seat experiences the constraint's decline as cognitive/social collapse (dueling becomes unthinkable despite their commitment; the regime they depended on evaporates across multiple registers simultaneously). The state-legal-system seat experiences it as institutional victory (the monopoly on settlement is consolidated; alternatives are eliminated; authority is concentrated). The merchant-professional seat experiences it as irrelevant background change (they never practiced dueling; they benefit from legal settlement machinery as a coordination mechanism). The analytical seat sees overdetermined decline: no single pathway is sufficient, but all pathways reinforce each other. The engine computes per-seat classifications: from the gentry-practitioner position, the regime computes as snare (high extraction, high suppression, identity-locked exit, high resistance that fails). From the state position, it computes as rope (coordination function—settling disputes without state machinery in early modernity—plus institutional benefit of consolidating settlement authority). From the merchant position, it computes as rope (the legal alternative is genuinely coordinating and beneficial). The divergence across seats is structural, not a measurement error.
 *
 * DIRECTIONALITY LOGIC:
 *   Gentry practitioners: d ≈ 0.95 (full target). They bear extraction (time, wealth, life risk), face rising suppression (legal prohibition, cultural delegitimation, organizational isolation), and hold identity-locked exit (for early practitioners, dueling was constitutive of self-concept and social position; exit meant status death even if the person survived). State-legal-system: d ≈ 0.05 (full beneficiary). It collects institutional authority, faces minimal suppression (it is the suppression infrastructure itself), and has arbitrage-grade exit options (legal authority consolidation is institutionally profitable and can expand into other domains). Emerging bourgeoisie: d ≈ 0.25 (partial beneficiary). They benefit from legal settlement machinery but also experience modest suppression if they are socially pressured to duel; they have mobile exit options (refuse dueling on grounds of class/profession; legal system backs them up). Religious institutions: d ≈ 0.15 (minor beneficiary). They benefit from moral authority over settlement and life-or-death decisions; suppression is minimal (they are part of the suppressing coalition). Analytical observer: d ≈ 0.50 (symmetric). They neither collect nor bear extraction; they measure and report.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—how to settle disputes between gentlemen without invoking state authority, preserving honor and status in the settlement process—is structurally DEAD by t=4 (interval end). Honor has been redescribed as barbaric; the state monopoly on dispute settlement is consolidated; legal courts have absorbed the settlement function; economic status no longer flows from combat capability. Yet the constraint persists in vestigial form: residual honor-culture practitioners continue sporadic dueling despite complete delegitimation. The theater_ratio rises (0.08 to 0.21) and then slightly declines (to 0.18) as the residual practice becomes pure spectral performance—aestheticized, identity-locked, functionally pointless. Mandatrophy exists but is **contested** in the reading terms: some observers claim dueling is functionally dead (mandatrophy resolved); others claim it persists in cultural/institutional residual form among subpopulations. The composite_reading declares mandatrophy exists but is **not yet fully resolved** at t=4 because the practice has not entirely disappeared—it persists as identity-locked fringe behavior. A full mandatrophy resolution would require either (a) the complete eradication of dueling practitioners, or (b) the cognitive impossibility of dueling even for identity-locked adherents (the human capacity to imagine and enact the practice must be erased, not merely legal/social pressure against it).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causality_attribution_ambiguity,
    'Is dueling''s decline driven primarily by cultural unthinkability (contraction) or by converging material/institutional suppression mechanisms, or by genuine mutual reinforcement of both pathways?',
    'Comparative historical analysis across jurisdictions with identical material/institutional changes but different cultural frameworks (where cultural unthinkability did NOT occur). Measurement: persistence of dueling among honor adherents in contexts of material suppression but intact cultural legitimacy vs. rapid abandonment where cultural delegitimation but weak material enforcement.',
    'If primarily cultural: reading shifts toward pure contraction (contraction_reading dominates). If primarily material: composite reading becomes misattributed; the real constraint is institutional suppression riding on a vestigial honor culture that was already eroding. If genuine mutual reinforcement: composite_reading classification holds; neither pathway is sufficient alone.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(causality_attribution_ambiguity, empirical, 'Whether dueling''s decline is overdetermined (all pathways independent + mutually reinforcing) or whether one pathway is sufficient and others are side effects.').

omega_variable(
    fringe_persistence_mechanism,
    'Among residual honor-culture practitioners who continued dueling after the cultural/legal collapse (drop_reading adherents), what sustained the practice: institutional isolation, identity-locked commitment, or active underground organization?',
    'Analysis of dueling incidents post-legal-prohibition in each jurisdiction: date of last recorded incidents, participant demographics, stated justifications, enforcement response intensity. Construct a coherence graph: did the practice disappear uniformly (simple collapse) or persist geographically/socially (residual subculture)? If persistent: interview/archival evidence on how practitioners sustained legitimacy.',
    'If practice persisted only through identity-locked commitment (duelers could not escape without rejecting their self-concept), the composite reading holds: cultural unthinkability is the primary driver and material suppression of a residual fringe is secondary. If practice persisted through organized subculture with internal legitimacy structures, drop_reading is the correct frame and the composite reading underestimates fringe resilience.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fringe_persistence_mechanism, empirical, 'The character of dueling''s persistence (or absence) in the post-legal-prohibition era.').

omega_variable(
    reading_frame_grid_selection,
    'This reading frames dueling as a constraint with composite decline mechanisms. An alternative framing treats each mechanism (cultural contraction, legal prohibition, social stratification change, economic transformation) as a separate constraint linked by contamination network edges. Which framing is more analytically tractable for predicting decline timing and residual adherence?',
    'Construct both models: (1) composite_reading as a single ε-invariant constraint with coercion grid at (macro) structural level showing multiple mechanisms in concert; (2) decomposed-reading as a constraint family with separate stories per mechanism, linked via network.affects_constraints. Run prediction against historical data: which model better predicts (a) timing of legal prohibitions relative to cultural shifts, (b) geographic variation in persistence, (c) demographic patterns of last-wave practitioners.',
    'If composite model predicts better: this reading''s ε-invariance holds and the constraint is genuinely overdetermined. If decomposed model predicts better: the composite reading conflates separate constraints; rewrite as a family with explicit causality edges and reevaluate each member''s ε independently.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_frame_grid_selection, conceptual, 'Whether overdetermined decline should be modeled as a single composite constraint or decomposed into a linked family.').

omega_variable(
    legitimacy_kernel_referent_drift,
    'The kernel ''honor_settlement_legitimacy'' is ambiguous across readings: does it refer to (A) the institutional arrangement that allowed honor to settle interpersonal disputes without state intervention, (B) the cultural authority of honor-based settlement as legitimate compared to legal settlement, or (C) the cognitive schema that made honor settlement thinkable at all? The composite_reading frame selects (C); does this selection hide treatment of (A) or (B)?',
    'For each reading, state explicitly which kernel referent it uses. If the sibling readings select different referents, each reading instantiates a different constraint (ε-invariance violation). If siblings select the same referent: declare this omega moot (resolved by the kernel structure itself).',
    'If referents differ: each reading has a defensible ε but they are not true siblings — the contest is between separate constraints wearing the same label. Reclassify as constraint family rather than kernel-reading set and write separate network linkage. If referents align: the composite_reading''s claim stands; sibling readings are competing causal stories of the same constraint''s fate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_kernel_referent_drift, conceptual, 'Whether the kernel referent is stable across readings or whether readings instantiate different constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_settlement_legitimacy__composite_reading, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dueling_theater_t0_functional, honor_settlement_legitimacy__composite_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(dueling_theater_t1_performative_onset, honor_settlement_legitimacy__composite_reading, theater_ratio, 1, 0.11).
narrative_ontology:measurement(dueling_theater_t2_ritual_emphasis, honor_settlement_legitimacy__composite_reading, theater_ratio, 2, 0.17).
narrative_ontology:measurement(dueling_theater_t3_pure_status_play, honor_settlement_legitimacy__composite_reading, theater_ratio, 3, 0.21).
narrative_ontology:measurement(dueling_theater_t4_spectral_performance, honor_settlement_legitimacy__composite_reading, theater_ratio, 4, 0.18).

% Extraction over time
narrative_ontology:measurement(dueling_extractiveness_t0_early_modern, honor_settlement_legitimacy__composite_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(dueling_extractiveness_t1_cultural_strain, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1, 0.51).
narrative_ontology:measurement(dueling_extractiveness_t2_legal_assault, honor_settlement_legitimacy__composite_reading, base_extractiveness, 2, 0.56).
narrative_ontology:measurement(dueling_extractiveness_t3_composite_peak, honor_settlement_legitimacy__composite_reading, base_extractiveness, 3, 0.62).
narrative_ontology:measurement(dueling_extractiveness_t4_residual_fringe, honor_settlement_legitimacy__composite_reading, base_extractiveness, 4, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(dueling_suppression_t0_minimal, honor_settlement_legitimacy__composite_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(dueling_suppression_t1_legal_prohibition, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1, 0.24).
narrative_ontology:measurement(dueling_suppression_t2_enforcement_intensification, honor_settlement_legitimacy__composite_reading, suppression_requirement, 2, 0.41).
narrative_ontology:measurement(dueling_suppression_t3_compound_enforcement, honor_settlement_legitimacy__composite_reading, suppression_requirement, 3, 0.58).
narrative_ontology:measurement(dueling_suppression_t4_residual_enforcement, honor_settlement_legitimacy__composite_reading, suppression_requirement, 4, 0.44).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=4
narrative_ontology:measurement(hono_grid_01, honor_settlement_legitimacy__composite_reading, accessibility_collapse(class), 0, 0.51).
narrative_ontology:measurement(hono_grid_02, honor_settlement_legitimacy__composite_reading, accessibility_collapse(class), 4, 0.92).
narrative_ontology:measurement(hono_grid_03, honor_settlement_legitimacy__composite_reading, accessibility_collapse(individual), 0, 0.35).
narrative_ontology:measurement(hono_grid_04, honor_settlement_legitimacy__composite_reading, accessibility_collapse(individual), 4, 0.89).
narrative_ontology:measurement(hono_grid_05, honor_settlement_legitimacy__composite_reading, accessibility_collapse(organizational), 0, 0.38).
narrative_ontology:measurement(hono_grid_06, honor_settlement_legitimacy__composite_reading, accessibility_collapse(organizational), 4, 0.88).
narrative_ontology:measurement(hono_grid_07, honor_settlement_legitimacy__composite_reading, accessibility_collapse(structural), 0, 0.42).
narrative_ontology:measurement(hono_grid_08, honor_settlement_legitimacy__composite_reading, accessibility_collapse(structural), 4, 0.91).
narrative_ontology:measurement(hono_grid_09, honor_settlement_legitimacy__composite_reading, resistance(class), 0, 0.61).
narrative_ontology:measurement(hono_grid_10, honor_settlement_legitimacy__composite_reading, resistance(class), 4, 0.31).
narrative_ontology:measurement(hono_grid_11, honor_settlement_legitimacy__composite_reading, resistance(individual), 0, 0.72).
narrative_ontology:measurement(hono_grid_12, honor_settlement_legitimacy__composite_reading, resistance(individual), 4, 0.28).
narrative_ontology:measurement(hono_grid_13, honor_settlement_legitimacy__composite_reading, resistance(organizational), 0, 0.74).
narrative_ontology:measurement(hono_grid_14, honor_settlement_legitimacy__composite_reading, resistance(organizational), 4, 0.22).
narrative_ontology:measurement(hono_grid_15, honor_settlement_legitimacy__composite_reading, resistance(structural), 0, 0.68).
narrative_ontology:measurement(hono_grid_16, honor_settlement_legitimacy__composite_reading, resistance(structural), 4, 0.18).
narrative_ontology:measurement(hono_grid_17, honor_settlement_legitimacy__composite_reading, stakes_inflation(class), 0, 0.41).
narrative_ontology:measurement(hono_grid_18, honor_settlement_legitimacy__composite_reading, stakes_inflation(class), 4, 0.71).
narrative_ontology:measurement(hono_grid_19, honor_settlement_legitimacy__composite_reading, stakes_inflation(individual), 0, 0.25).
narrative_ontology:measurement(hono_grid_20, honor_settlement_legitimacy__composite_reading, stakes_inflation(individual), 4, 0.62).
narrative_ontology:measurement(hono_grid_21, honor_settlement_legitimacy__composite_reading, stakes_inflation(organizational), 0, 0.32).
narrative_ontology:measurement(hono_grid_22, honor_settlement_legitimacy__composite_reading, stakes_inflation(organizational), 4, 0.68).
narrative_ontology:measurement(hono_grid_23, honor_settlement_legitimacy__composite_reading, stakes_inflation(structural), 0, 0.28).
narrative_ontology:measurement(hono_grid_24, honor_settlement_legitimacy__composite_reading, stakes_inflation(structural), 4, 0.74).
narrative_ontology:measurement(hono_grid_25, honor_settlement_legitimacy__composite_reading, suppression(class), 0, 0.18).
narrative_ontology:measurement(hono_grid_26, honor_settlement_legitimacy__composite_reading, suppression(class), 4, 0.54).
narrative_ontology:measurement(hono_grid_27, honor_settlement_legitimacy__composite_reading, suppression(individual), 0, 0.06).
narrative_ontology:measurement(hono_grid_28, honor_settlement_legitimacy__composite_reading, suppression(individual), 4, 0.52).
narrative_ontology:measurement(hono_grid_29, honor_settlement_legitimacy__composite_reading, suppression(organizational), 0, 0.12).
narrative_ontology:measurement(hono_grid_30, honor_settlement_legitimacy__composite_reading, suppression(organizational), 4, 0.61).
narrative_ontology:measurement(hono_grid_31, honor_settlement_legitimacy__composite_reading, suppression(structural), 0, 0.08).
narrative_ontology:measurement(hono_grid_32, honor_settlement_legitimacy__composite_reading, suppression(structural), 4, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_settlement_legitimacy__composite_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_settlement_legitimacy__composite_reading, 0.12).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__composite_reading, honor_settlement_legitimacy__contraction_reading).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__composite_reading, honor_settlement_legitimacy__drop_reading).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__composite_reading, status_competition_regime__genteel_period).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__composite_reading, legal_monopoly_on_dispute_settlement).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a contested kernel. The kernel honor_settlement_legitimacy has three competing readings: composite_reading (this story, framing decline as overdetermined), contraction_reading (decline driven by cultural delegitimation), and drop_reading (persistence of practice among residual adherents). Each reading instantiates a different constraint with different ε values and causal stories. They are linked as a constraint family via network.affects_constraints (each reading links to its siblings). The composite_reading differs from the others in positing genuine interdependence of multiple causal pathways rather than dominance of one pathway. To model the kernel dispute itself (not just the readings), author three separate constraint stories, one per reading, each with its own base_properties metrics and stakeholders. The three stories form a kernel family joined by reading_relations in cs_structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_settlement_legitimacy__composite_reading, institutional, 0.08).
constraint_indexing:directionality_override(honor_settlement_legitimacy__composite_reading, organized, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
