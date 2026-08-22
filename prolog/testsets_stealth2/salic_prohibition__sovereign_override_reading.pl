% ============================================================================
% CONSTRAINT STORY: salic_prohibition__sovereign_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_salic_prohibition__sovereign_override_reading, []).

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
 *   constraint_id: salic_prohibition__sovereign_override_reading
 *   human_readable: Pragmatic Sanction Succession Settlement (Sovereign-Override Reading)
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested salic_prohibition
 *   kernel: the sovereign_override_reading, under which the exclusion of
 *   females from dynastic succession is revocable positive law, amendable by
 *   sovereign legislative act. Its historical body is the Pragmatic Sanction
 *   of 1713 and its armed defense: Charles VI legislates female succession
 *   and indivisibility, buys assent and guarantees for two decades, and when
 *   Frederick II invades Silesia in 1740 the settlement's answer is defensive
 *   war - challengers are rebels against legitimate authority, not
 *   interpreters of ancient law. The epsilon referent is the standing
 *   arrangement under contest, the sanction-backed succession order as it
 *   actually operated, assessed by this reading's own lights: even a reading
 *   that endorses sovereign amendment registers the uncompensated
 *   displacement of the agnates and the war burdens laid on estates and
 *   subjects as real costs of the arrangement. The sibling readings
 *   (immutable_mandate_reading, cognatic_reversion_reading) are separate
 *   constraint stories with their own epsilon values, linked via
 *   network.affects_constraints. KEY AGENTS (by structural relationship): -
 *   sovereign_legislator_charles_vi: Agenda-setter (institutional/arbitrage)
 *   - authors and can amend the settlement, trades concessions for assent -
 *   designated_heiress_line: Primary beneficiary (powerful/identity_locked) -
 *   holds the succession the settlement opens; cannot exit without dissolving
 *   her own title - habsburg_dynastic_center: Secondary beneficiary
 *   (institutional/constrained) - administers settlement, avoids partition -
 *   displaced_agnate_claimants: Primary target (powerful/constrained) -
 *   claims set aside without compensation - hungarian_crown_estates: Target
 *   with extracted concessions (organized/constrained) - pays troops and
 *   taxes, extracts constitutional price - common_dynastic_subjects: Diffuse
 *   target (powerless/trapped) - bears levies, quarters, and casualties -
 *   european_guarantor_powers: Paid counterparties turned belligerents
 *   (institutional/mobile) - public_law_jurists: Analytical observer -
 *   supplies the doctrinal arguments all seats borrow
 *
 * KEY AGENTS:
 *   - sovereign_legislator_charles_vi: agenda-setter (institutional/arbitrage) - authors, sells, and could re-legislate the settlement
 *   - designated_heiress_line: primary beneficiary (powerful/identity_locked) - the succession itself accrues here; exit equals self-dissolution
 *   - habsburg_dynastic_center: secondary beneficiary (institutional/constrained) - administration concentrated by non-partition
 *   - displaced_agnate_claimants: primary target (powerful/constrained) - uncompensated displacement, recourse to arms or litigation
 *   - hungarian_crown_estates: target with dual position (organized/constrained) - war contributions out, constitutional concessions back
 *   - common_dynastic_subjects: diffuse target (powerless/trapped) - levies, quarters, casualties, no assent solicited
 *   - european_guarantor_powers: counterparties (institutional/mobile) - priced signatures, defection available
 *   - public_law_jurists: analytical observer (analytical/analytical) - doctrinal debate, adjudicates nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(salic_prohibition__sovereign_override_reading, 0.63).
domain_priors:suppression_score(salic_prohibition__sovereign_override_reading, 0.7).
domain_priors:theater_ratio(salic_prohibition__sovereign_override_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, extractiveness, 0.63).
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(salic_prohibition__sovereign_override_reading, tangled_rope).
narrative_ontology:human_readable(salic_prohibition__sovereign_override_reading, "Pragmatic Sanction Succession Settlement (Sovereign-Override Reading)").
narrative_ontology:topic_domain(salic_prohibition__sovereign_override_reading, "constitutional/political").

domain_priors:requires_active_enforcement(salic_prohibition__sovereign_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(salic_prohibition__sovereign_override_reading, 'd16abb2d-424c-4f1a-98f5-70a10a566d4a').
narrative_ontology:cs_kernel_codification('d16abb2d-424c-4f1a-98f5-70a10a566d4a', formalized).
narrative_ontology:cs_authority_grounding('d16abb2d-424c-4f1a-98f5-70a10a566d4a', lineage).
narrative_ontology:cs_interpretation_layer_present('d16abb2d-424c-4f1a-98f5-70a10a566d4a').
narrative_ontology:cs_reading_relation('d16abb2d-424c-4f1a-98f5-70a10a566d4a', salic_prohibition__immutable_mandate_reading, forecloses).
narrative_ontology:cs_reading_relation('d16abb2d-424c-4f1a-98f5-70a10a566d4a', salic_prohibition__cognatic_reversion_reading, influences).
narrative_ontology:cs_axiom('d16abb2d-424c-4f1a-98f5-70a10a566d4a', foundational, succession_rule_amendable_by_sovereign_legislative_act).
narrative_ontology:cs_axiom_status(succession_rule_amendable_by_sovereign_legislative_act, holdable).
narrative_ontology:cs_axiom_grounding('d16abb2d-424c-4f1a-98f5-70a10a566d4a', succession_rule_amendable_by_sovereign_legislative_act, conventional).
narrative_ontology:cs_axiom('d16abb2d-424c-4f1a-98f5-70a10a566d4a', secondary, armed_challenge_to_designated_succession_is_rebellion).
narrative_ontology:cs_axiom_status(armed_challenge_to_designated_succession_is_rebellion, holdable).
narrative_ontology:cs_axiom_grounding('d16abb2d-424c-4f1a-98f5-70a10a566d4a', armed_challenge_to_designated_succession_is_rebellion, conventional).
narrative_ontology:cs_reference_frame('d16abb2d-424c-4f1a-98f5-70a10a566d4a', revocable_positive_law_regime).
narrative_ontology:cs_drift_state('d16abb2d-424c-4f1a-98f5-70a10a566d4a', aix_la_chapelle_settlement, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d16abb2d-424c-4f1a-98f5-70a10a566d4a', '').
narrative_ontology:cs_kernel_id(salic_prohibition__sovereign_override_reading, salic_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(salic_prohibition__sovereign_override_reading, designated_heiress_line).
narrative_ontology:constraint_beneficiary(salic_prohibition__sovereign_override_reading, habsburg_dynastic_center).
narrative_ontology:constraint_victim(salic_prohibition__sovereign_override_reading, displaced_agnate_claimants).
narrative_ontology:constraint_victim(salic_prohibition__sovereign_override_reading, hungarian_crown_estates).
narrative_ontology:constraint_victim(salic_prohibition__sovereign_override_reading, common_dynastic_subjects).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(salic_prohibition__sovereign_override_reading, hungarian_crown_estates).
narrative_ontology:constraint_beneficiary(salic_prohibition__sovereign_override_reading, european_guarantor_powers).
narrative_ontology:constraint_victim(salic_prohibition__sovereign_override_reading, european_guarantor_powers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Reigns over the Habsburg crowns with no surviving son. Drafts and promulgates the Pragmatic Sanction in 1713, declaring the lands indivisible and admitting his daughters to the succession, then spends two decades obtaining oaths of assent from his own estates and written guarantees from foreign courts, paying for each signature with trade concessions, territorial promises, and marriage diplomacy. Retains the power to convoke, amend, or reissue the settlement; his personal standing rises and falls with its acceptance abroad.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, sovereign_legislator_charles_vi, agenda_setter,
    institutional, generational, arbitrage, continental).

% Maria Theresa and her descendants hold the succession the Sanction opens. Her title exists only while the settlement is honored; she inherits the war launched to test it, loses Silesia to the invader, rallies the Hungarian estates with a 1741 plea for life and blood, and secures recognition at Aix-la-Chapelle in 1748. Renouncing the settlement would dissolve her own claim; defending it consumes the reign's military and fiscal capacity.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, designated_heiress_line, beneficiary,
    powerful, generational, identity_locked, continental).

% The court chancery, treasury, and army command administer the settlement: registering homages, negotiating and renewing guarantees, and waging the defensive war. The center avoids the partition that would have devolved revenues, regiments, and offices to branch courts, concentrating them in Vienna instead; its personnel and procedures are built around the settlement's maintenance.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, habsburg_dynastic_center, beneficiary,
    institutional, generational, constrained, continental).

% The Wittelsbach elector of Bavaria, the Saxon elector, and the Spanish Bourbon line hold male-line descent claims that the Sanction sets aside without compensation. Their options are pressing arms (the Bavarian elector is briefly elected Emperor Charles VII), trading recognition for guarantees (Saxony), or litigating genealogies before the chanceries of Europe. None can withdraw from the succession order's consequences.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, displaced_agnate_claimants, payer,
    powerful, biographical, constrained, continental).

% The Hungarian diet swears homage to the Sanction in 1722-23 only after extracting reaffirmation of the Golden Bull's liberties, coronation with the crown of Saint Stephen, and recognition of Hungary's distinct constitutional standing. It then fields regiments and taxes for the defensive war, and its 1741 pledge of life and blood helps save the queen's cause, at the price of renewed privilege confirmations.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, hungarian_crown_estates, payer,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(salic_prohibition__sovereign_override_reading, hungarian_crown_estates, beneficiary).

% Peasants, townsmen, and rank-and-file soldiers across the crownlands pay the war levies, quarter armies, fill the regiments, and absorb the requisitions and epidemics that follow campaigning in Bohemia, Bavaria, and Italy. They appear in the settlement only as oath-takers and taxpayers; no clause of the Sanction solicits their assent.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, common_dynastic_subjects, payer,
    powerless, immediate, trapped, continental).

% France, Britain, the Dutch Republic, Russia, Spain, Prussia, and leading imperial estates sign written guarantees of the settlement through the 1720s and 1730s, each pricing its signature in concessions: commerce treaties, barrier fortresses, dowries, subsidies. When the test comes in 1740, Prussia seizes Silesia, France backs the Bavarian candidature, Spain presses Italian claims, while Britain and the Dutch fund the defense. Signature and defection remain equally available moves throughout.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, european_guarantor_powers, beneficiary,
    institutional, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(salic_prohibition__sovereign_override_reading, european_guarantor_powers, payer).

% University jurists, chancery counselors, and publicists across Europe debate whether a sovereign act can vary a succession embedded in provincial constitutions, whether fundamental laws differ from ordinary statutes, and what the public-law corpus says about female succession. They adjudicate nothing themselves but supply the arguments every court borrows.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, public_law_jurists, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(salic_prohibition__sovereign_override_reading, designated_heiress_line).
narrative_ontology:fixing_cost_class(salic_prohibition__sovereign_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixes a determinate, unitary succession order for a composite monarchy whose ruler has no surviving son: it gives the crownlands' estates a single counterparty for homage, gives foreign courts a single instrument to guarantee, and prevents the partition of the patrimony among branch courts and neighboring powers.
% TRANSFER_FUNCTION: Moves succession rights and territorial integrity from the displaced agnate claimants to the designated heiress's line; moves taxes, troops, and diplomatic concessions from the crownland estates and the guarantor powers to the dynastic center, in exchange for recognition and confirmed privileges.
% ABSENT_VOICES: The agnates were approached late and by pressure, not consent; their objections survive in Munich, Dresden, and Madrid correspondence. Common subjects had no voice at all in an arrangement they financed with blood and taxes. Women outside the dynastic exception were nowhere consulted on the rule that governed them.
% DISAPPEARANCE_RATIONALE: If the settlement vanished overnight, the agnate claims revive immediately, the partition schemes of 1668 and the Spanish-precedent carve-ups return to the table, and the monarchy's unity gets renegotiated by treaty or by force among Vienna, Munich, Dresden, Berlin, and Versailles. The arrangement, plus the arms that defended it, is what held the inheritance together.
% FOUNDING_PROBLEM: Secure an undivided Habsburg succession for a ruler with no surviving son, against branch courts' partition expectations and neighbors' appetite for carved portions, on the precedent of the 1668 partition treaty projects and the Spanish inheritance collapse of 1700.
% FOUNDING_PROBLEM_CORROBORATION: Rival courts' diplomatic correspondence (Munich, Dresden, Versailles, Berlin) attests the succession-security problem was real and unresolved by consent; the Aix-la-Chapelle congress records treat the settlement as ratified by arms rather than accepted as law; Hungarian diet deliberations attest the estates' assent was conditional and purchased. Attestation therefore does not rest on the beneficiary circle alone.
narrative_ontology:disappearance_verdict(salic_prohibition__sovereign_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(salic_prohibition__sovereign_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(salic_prohibition__sovereign_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(salic_prohibition__sovereign_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(salic_prohibition__sovereign_override_reading, 0.63, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(salic_prohibition__sovereign_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(salic_prohibition__sovereign_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(salic_prohibition__sovereign_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.63 at interval end) is substantial but bounded: the settlement extinguishes identifiable parties' succession expectations without compensation and finances a seven-year general war, yet it also delivers the anti-partition good a composite monarchy with an empty male line genuinely needed. Suppression (0.70) reflects the enforcement reality: the settlement bound only through coerced homage sequences, purchased guarantees, and finally military victory; the trajectory steps sharply at 1740 when enforcement turns kinetic. Theater (0.28) tracks the guarantee era, where signature ceremonies multiplied faster than sincerity - the ratio peaks in 1738 as hollow guarantees accumulate, dips when real fighting replaces ceremony in 1740, and settles moderately. Accessibility collapse (0.55): partition schemes and elective alternatives were real before promulgation and were largely closed off by the sworn settlement, but never fully - the war itself was the collapsed alternative reopening. Resistance (0.75) is unusually high because the targets included sovereign princes capable of fielding armies. All three series run on one shared time grid (1713, 1720, 1731, 1738, 1740, 1745, 1748) so every metric is authored at every examined point; the 1740 step-change is a war-onset discontinuity, not noise. The claimed type (tangled_rope) is authored from structural belief - genuine coordination function plus asymmetric extraction plus active enforcement - independently of these metric values.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the agenda-setter's chair the settlement is coordination he authored: filial provision fused with statecraft, the alternative being partition and foreign carve-up. From the agnate seat the same instrument is dispossession by unilateral act, answered by arms. From the subject seat it is taxation and conscription for a quarrel over someone else's inheritance. From the guarantor seats it is a market in signatures with defection priced in. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the heiress line and dynastic center toward the subsidy end; the heiress's identity_locked exit pins her at the deepest beneficiary position since her title and the settlement are the same thing. Victim declarations drive the agnates and common subjects toward the full-target end - the agnates are powerful but constrained (arms or litigation, no neutral exit), the subjects powerless and trapped. The Hungarian estates are the deliberate complication: the derivation from victim + organized + constrained would place them near full target, but their situation documents a real reverse flow (Golden Bull reaffirmation, coronation, privilege confirmations extracted as the price of homage), so a directionality override moves the organized seat down to 0.72. The guarantor powers sit near-symmetric by construction - they collected concessions for signatures and then split between honoring (subsidy payers) and defecting (belligerents); no override is applied because the class genuinely straddles.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - securing an undivided succession for a ruler without a son - was live at promulgation and its urgency passed within a generation (a male heir was born in 1741), yet the arrangement persists past its trigger as the indivisibility doctrine and the enforcement apparatus built to defend it. Classifying this as tangled_rope rather than snare prevents mislabeling the genuine anti-partition coordination as pure extraction; authoring theater_ratio honestly guards against the guarantee-ceremony layer hardening into pure performance once the war ends. The founding_problem_status is authored 'contested' rather than 'dead' because the deeper function (unitary succession security for a multi-crown monarchy) remained live into the next generation; the status-times-verdict pair stays off the automatic zombie flag, which is the honest reading rather than a flattering one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_contestation_of_salic_kernel,
    'This constraint instantiates the sovereign_override_reading of the salic_prohibition kernel; how would adoption of a sibling reading (immutable_mandate_reading, cognatic_reversion_reading) restructure the arrangement?',
    'Cross-file comparison of the three reading stories: differ the enforcement basis (divine sanction versus never-binding custom versus legislative act), the challenger taxonomy (heretic versus anachronism-survivor versus rebel), and recompute per-seat classifications under each.',
    'Under immutable_mandate_reading the settlement''s enforcement rests on sacral obligation and the sovereign loses amendment authority, demoting the agenda-setter seat; under cognatic_reversion_reading the prohibition dissolves as never-bound and the enforcement machinery loses its object entirely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contestation_of_salic_kernel, conceptual, 'Committer structure: this story is one of three readings of the Salic-prohibition kernel; sibling readings are separate constraints.').

omega_variable(
    guarantee_sincerity,
    'Were the great-power guarantees of the Pragmatic Sanction sincere commitments or priced, defeasible instruments from the outset?',
    'Archival comparison of secret cabinet instructions (Versailles, Berlin, Saint Petersburg) against the public guarantee instruments, focusing on reservation clauses and contingency planning dated before 1740.',
    'If insincere from the start, the guarantee phase was ceremonial cover for eventual partition, raising theater_ratio and pushing the arrangement toward pure extraction; if sincere, the coordination function is stronger and the defections are contingent shocks rather than design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(guarantee_sincerity, empirical, 'Whether the guarantee layer was commitment or theater.').

omega_variable(
    estate_assent_character,
    'Did the crownland estates'' assent to the Sanction express consent to the amended succession order, or formal submission under duress?',
    'Compare diet deliberation records (Hungarian 1722-23, Bohemian, Austrian) with the concessions each estate extracted and the latency of subsequent compliance.',
    'If assent was coerced formality, the estates'' contributions belong on the cost-bearing side without offset and measured suppression understates the true figure; if a genuine bargain, part of the measured extraction is coordinated cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(estate_assent_character, empirical, 'Consent versus duress in the estate homage sequence.').

omega_variable(
    uncompensated_displacement_legitimacy,
    'Is extinguishing the agnates'' succession expectations by unilateral legislative act a legitimate exercise of sovereign authority or an uncompensated taking?',
    'Normative analysis: the answer turns on whether dynastic succession expectations are property-like rights or revocable grants - a values question on which the period''s own jurists divided.',
    'Resolving toward ''taking'' pushes the classification from tangled_rope toward snare; resolving toward ''legitimate grant-revision'' supports the coordination reading and lowers effective extraction for the sovereign seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(uncompensated_displacement_legitimacy, preference, 'Values-dependent status of the displaced claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(salic_prohibition__sovereign_override_reading, 1713, 1748).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(salic_sov_override_tr_t1713, salic_prohibition__sovereign_override_reading, theater_ratio, 1713, 0.14).
narrative_ontology:measurement(salic_sov_override_tr_t1720, salic_prohibition__sovereign_override_reading, theater_ratio, 1720, 0.18).
narrative_ontology:measurement(salic_sov_override_tr_t1731, salic_prohibition__sovereign_override_reading, theater_ratio, 1731, 0.23).
narrative_ontology:measurement(salic_sov_override_tr_t1738, salic_prohibition__sovereign_override_reading, theater_ratio, 1738, 0.29).
narrative_ontology:measurement(salic_sov_override_tr_t1740, salic_prohibition__sovereign_override_reading, theater_ratio, 1740, 0.24).
narrative_ontology:measurement(salic_sov_override_tr_t1745, salic_prohibition__sovereign_override_reading, theater_ratio, 1745, 0.27).
narrative_ontology:measurement(salic_sov_override_tr_t1748, salic_prohibition__sovereign_override_reading, theater_ratio, 1748, 0.28).

% Extraction over time
narrative_ontology:measurement(salic_sov_override_be_t1713, salic_prohibition__sovereign_override_reading, base_extractiveness, 1713, 0.44).
narrative_ontology:measurement(salic_sov_override_be_t1720, salic_prohibition__sovereign_override_reading, base_extractiveness, 1720, 0.49).
narrative_ontology:measurement(salic_sov_override_be_t1731, salic_prohibition__sovereign_override_reading, base_extractiveness, 1731, 0.53).
narrative_ontology:measurement(salic_sov_override_be_t1738, salic_prohibition__sovereign_override_reading, base_extractiveness, 1738, 0.56).
narrative_ontology:measurement(salic_sov_override_be_t1740, salic_prohibition__sovereign_override_reading, base_extractiveness, 1740, 0.66).
narrative_ontology:measurement(salic_sov_override_be_t1745, salic_prohibition__sovereign_override_reading, base_extractiveness, 1745, 0.65).
narrative_ontology:measurement(salic_sov_override_be_t1748, salic_prohibition__sovereign_override_reading, base_extractiveness, 1748, 0.63).

% Suppression requirement over time
narrative_ontology:measurement(salic_sov_override_su_t1713, salic_prohibition__sovereign_override_reading, suppression_requirement, 1713, 0.38).
narrative_ontology:measurement(salic_sov_override_su_t1720, salic_prohibition__sovereign_override_reading, suppression_requirement, 1720, 0.46).
narrative_ontology:measurement(salic_sov_override_su_t1731, salic_prohibition__sovereign_override_reading, suppression_requirement, 1731, 0.52).
narrative_ontology:measurement(salic_sov_override_su_t1738, salic_prohibition__sovereign_override_reading, suppression_requirement, 1738, 0.57).
narrative_ontology:measurement(salic_sov_override_su_t1740, salic_prohibition__sovereign_override_reading, suppression_requirement, 1740, 0.74).
narrative_ontology:measurement(salic_sov_override_su_t1745, salic_prohibition__sovereign_override_reading, suppression_requirement, 1745, 0.73).
narrative_ontology:measurement(salic_sov_override_su_t1748, salic_prohibition__sovereign_override_reading, suppression_requirement, 1748, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(salic_prohibition__sovereign_override_reading, resource_allocation).
narrative_ontology:affects_constraint(salic_prohibition__sovereign_override_reading, salic_prohibition__immutable_mandate_reading).
narrative_ontology:affects_constraint(salic_prohibition__sovereign_override_reading, salic_prohibition__cognatic_reversion_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Salic Law' decomposes into three structurally distinct constraints, one per reading of the kernel: irrevocable divine mandate, never-binding Frankish anachronism, and revocable positive law. Each carries its own epsilon, beneficiary/victim structure, and enforcement basis; this story is the third. Downstream structure: this reading's institutional victory (the Pragmatic Sanction) changed the legitimacy conditions under which the cognatic reading argues, giving sovereigns a working template for unilateral amendment, while directly contradicting the immutable-mandate reading's core premise about the same rule's modal status.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(salic_prohibition__sovereign_override_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
