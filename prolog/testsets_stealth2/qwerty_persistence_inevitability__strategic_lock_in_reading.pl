% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_inevitability__strategic_lock_in_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_inevitability__strategic_lock_in_reading, []).

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
 *   constraint_id: qwerty_persistence_inevitability__strategic_lock_in_reading
 *   human_readable: QWERTY Persistence as Manufacturer-Engineered Lock-In (Strategic Lock-In Reading)
 *   domain: technology_history/political_economy
 *
 * SUMMARY:
 *   This story instantiates the strategic_lock_in_reading of the contested
 *   kernel qwerty_persistence_inevitability: the claim that QWERTY's
 *   persistence was not an accident that compounded but an outcome
 *   manufactured by the firms best positioned to profit from it. On this
 *   reading the Sholes layout reached market through Remington in 1874, and
 *   between 1889 and 1893 the leading manufacturers merged into the Union
 *   Typewriter Company trust, controlling roughly ninety percent of United
 *   States output. The trust's standardization agreement fixed QWERTY across
 *   every member machine line; exclusive supply and referral arrangements
 *   bound typing schools to teach it; employers, hiring from a uniform
 *   graduate pool, demanded nothing else. The result was a closed loop of
 *   machines, schools, and hiring in which the layout's persistence was
 *   produced by deliberate coordination of the machine market and the
 *   training pipeline, its ergonomic and switching costs fell on the typists
 *   who operated it, and its benefits accrued to the member firms and their
 *   school partners. Per the epsilon-invariance discipline this file authors
 *   ONLY this reading: the sibling path_dependency_reading (persistence as
 *   accident compounded without strategic beneficiaries) is a separate
 *   constraint with its own epsilon, beneficiary structure, and type, linked
 *   through network.affects_constraints. The two readings disagree about
 *   intentionality and beneficiary structure, not about the physical layout
 *   or the historical sequence. KEY AGENTS (by structural relationship): -
 *   union_typewriter_trust_members: agenda-setter and principal collector
 *   (institutional/arbitrage) — wrote the 1893 standardization terms and drew
 *   the resulting market security - commercial_typing_schools: beneficiary
 *   (organized/constrained) — ran the QWERTY-only curriculum the training
 *   partnerships guaranteed - employing_offices: beneficiary
 *   (organized/constrained) — drew interchangeable QWERTY-trained labor from
 *   the standardized pipeline - professional_typists: primary target
 *   (powerless/trapped) — bore ergonomic costs and retraining barriers with
 *   no seat in standardization - non_qwerty_keyboard_manufacturers: secondary
 *   target (moderate/constrained) — lost dealer and school channels to the
 *   standardization agreement - alternative_layout_inventors: excluded voice
 *   (moderate/trapped) — no channel into procurement or curricula -
 *   technology_historians: analytical observer — reconstructs intentionality
 *   and cost distribution from archives
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.58).
domain_priors:suppression_score(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.34).
domain_priors:theater_ratio(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 0.34).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_inevitability__strategic_lock_in_reading, tangled_rope).
narrative_ontology:human_readable(qwerty_persistence_inevitability__strategic_lock_in_reading, "QWERTY Persistence as Manufacturer-Engineered Lock-In (Strategic Lock-In Reading)").
narrative_ontology:topic_domain(qwerty_persistence_inevitability__strategic_lock_in_reading, "technology_history/political_economy").

domain_priors:requires_active_enforcement(qwerty_persistence_inevitability__strategic_lock_in_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_inevitability__strategic_lock_in_reading, '18ec5bc1-5dd9-4629-aa83-29bfc913713b').
narrative_ontology:cs_kernel_codification('18ec5bc1-5dd9-4629-aa83-29bfc913713b', distributed).
narrative_ontology:cs_authority_grounding('18ec5bc1-5dd9-4629-aa83-29bfc913713b', expertise).
narrative_ontology:cs_interpretation_layer_present('18ec5bc1-5dd9-4629-aa83-29bfc913713b').
narrative_ontology:cs_reading_relation('18ec5bc1-5dd9-4629-aa83-29bfc913713b', qwerty_persistence_inevitability__path_dependency_reading, coexists_with).
narrative_ontology:cs_axiom('18ec5bc1-5dd9-4629-aa83-29bfc913713b', foundational, qwerty_persistence_was_manufacturer_engineered).
narrative_ontology:cs_axiom_status(qwerty_persistence_was_manufacturer_engineered, holdable).
narrative_ontology:cs_axiom_grounding('18ec5bc1-5dd9-4629-aa83-29bfc913713b', qwerty_persistence_was_manufacturer_engineered, empirically_contingent).
narrative_ontology:cs_axiom('18ec5bc1-5dd9-4629-aa83-29bfc913713b', secondary, standardization_costs_externalized_onto_typists).
narrative_ontology:cs_axiom_status(standardization_costs_externalized_onto_typists, holdable).
narrative_ontology:cs_axiom_grounding('18ec5bc1-5dd9-4629-aa83-29bfc913713b', standardization_costs_externalized_onto_typists, empirically_contingent).
narrative_ontology:cs_reference_frame('18ec5bc1-5dd9-4629-aa83-29bfc913713b', cartel_engineered_standardization_regime).
narrative_ontology:cs_drift_state('18ec5bc1-5dd9-4629-aa83-29bfc913713b', post_liebowitz_margolis_critique, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('18ec5bc1-5dd9-4629-aa83-29bfc913713b', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_inevitability__strategic_lock_in_reading, qwerty_persistence_inevitability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__strategic_lock_in_reading, union_typewriter_trust_members).
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__strategic_lock_in_reading, commercial_typing_schools).
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__strategic_lock_in_reading, employing_offices).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__strategic_lock_in_reading, professional_typists).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__strategic_lock_in_reading, non_qwerty_keyboard_manufacturers).
narrative_ontology:constraint_vindicates(qwerty_persistence_inevitability__strategic_lock_in_reading, strategic_lockin_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A merger of the five leading American typemaker firms (Remington, Yost, Densmore, Caligraph, Smith Premier) completed in 1893, controlling roughly nine-tenths of United States output. Member firms signed a standardization agreement fixing the Sholes/QWERTY layout across every machine line, ran exclusive supply and referral arrangements with typing schools, and coordinated dealer terms. Setting the standard was theirs; the agreement protected member margins against layout competition and kept rival designs out of the school and dealer channels. Leaving the arrangement was cheap for them: they wrote the terms and could have revised them.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, union_typewriter_trust_members, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence_inevitability__strategic_lock_in_reading, union_typewriter_trust_members, beneficiary).

% Private commercial schools that taught machine typing to young women entering clerical work. Under supply and referral arrangements with the manufacturers they taught the QWERTY layout exclusively, because employers hired only QWERTY-trained graduates. Standardization guaranteed them a uniform curriculum and steady enrollment; they collected tuition from the arrangement but held no seat in the manufacturers' standardization decisions, and their entire product line consisted of teaching whatever layout the machine market settled on.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, commercial_typing_schools, beneficiary,
    organized, biographical, constrained, national).

% Businesses, railroads, courts, and government offices that hired typists. A single layout let them hire interchangeably from any school's graduates, substitute operators at will, and buy machines without layout-specific training costs. They paid ordinary wages and equipment prices and carried none of the retraining burden; their interest lay in whatever layout the labor market already spoke, and no graduate pool existed in any other.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, employing_offices, beneficiary,
    organized, biographical, constrained, national).

% The overwhelmingly young, female clerical workforce operating the machines eight to ten hours a day. They bore the layout's ergonomic costs, including awkward finger loads and constant reaches to the top row, along with its speed ceiling, and any move to a different layout would have meant months of unpaid retraining and loss of certified speed. Changing employers never changed the layout, since every employer hired the same one. They had no representation in the manufacturers' association, the schools, or the standardization agreement, and their preferences entered the record mainly through turnover and complaint.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, professional_typists, payer,
    powerless, biographical, trapped, national).

% Typemaker firms outside the trust, such as Crandall, Munson, and Blickensderfer with its own layouts, who found dealers refusing stock, schools refusing to teach their machines, and buyers unwilling to train operators on a minority layout. Their choices were to join the trust on its terms, abandon layout development, or leave the business; most took one of the first two.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, non_qwerty_keyboard_manufacturers, payer,
    moderate, biographical, constrained, national).

% Inventors of redesigned keyboards, from early simplified layouts through Dvorak's 1936 patent, who argued the standard layout wasted motion and strained hands. Their designs required simultaneous adoption by manufacturers, schools, and employers, none of whom would move first while the installed base of machines and trained typists made any rival layout commercially irrational. They had no channel into procurement specifications or school curricula, and their proposals died in committee and correspondence.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, alternative_layout_inventors, excluded,
    moderate, biographical, trapped, national).

% Economic and technology historians reconstructing the standardization decisions from corporate archives, trade press, and patent records. They assess whether the 1893 agreement was adopted to solve fragmentation, to protect member margins, or both, and they trace where the costs of the settled layout landed. They hold no position in the arrangement, and their findings feed the scholarly dispute rather than any party's operations.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, technology_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence_inevitability__strategic_lock_in_reading, union_typewriter_trust_members).
narrative_ontology:fixing_cost_class(qwerty_persistence_inevitability__strategic_lock_in_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A single keyboard layout solved a real multi-party coordination problem: typist skills became portable between employers, machines and supplies interoperated, schools ran one curriculum instead of one per manufacturer, and employers could replace a departing operator without retraining. Whatever its origins, the standard performed this coordinating work across half a century of clerical expansion.
% TRANSFER_FUNCTION: The standardization agreement moved market security and margin protection to the five member manufacturers, guaranteed enrollment revenue to affiliated typing schools, and delivered hiring convenience to employers; it moved the costs, meaning an ergonomically costly finger-load pattern, a speed ceiling, insurmountable retraining barriers, and the foreclosure of rival designs, onto typists and onto manufacturers and inventors outside the agreement.
% ABSENT_VOICES: Typists, the thousands of young women doing the work the standard organized, had no seat in the manufacturers' association, the schools' referral councils, or the standardization negotiations; their objections survive only as turnover statistics and scattered complaints. Inventors of rival layouts were likewise outside the room, with no vote in dealer terms or school curricula. Both groups would have objected to having the layout chosen on machine-market grounds alone.
% DISAPPEARANCE_RATIONALE: Overnight disappearance would fragment the clerical labor market by layout: schools would split curricula, employers would re-specify hiring, manufacturers would reopen layout competition, and the certified-speed credentials of every working typist would depreciate at a stroke. The arrangement is load-bearing for the labor market it created, whoever bears its costs.
% FOUNDING_PROBLEM: In the 1880s every manufacturer shipped a different keyboard layout, skilled operators could not move between machines, and each firm feared a rival's layout becoming the labor-market standard and shutting its own machines out of offices. The 1893 standardization agreement was built to end that fragmentation and to make sure the surviving layout belonged to the member firms.
% FOUNDING_PROBLEM_CORROBORATION: Business historians working from Union Typewriter Company archives and the trade press of 1889-1894 corroborate both the fragmentation problem and its resolution; ergonomics and human-factors literature independently attests that the costs the settlement declined to pay did not disappear but moved to the operators. No party inside the arrangement attests the founding problem as still live: the machine-fragmentation problem ended with the mechanical-typewriter era, and what persists is a generalized any-layout compatibility need that no one traces to the 1893 agreement specifically.
narrative_ontology:disappearance_verdict(qwerty_persistence_inevitability__strategic_lock_in_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_inevitability__strategic_lock_in_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_inevitability__strategic_lock_in_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(qwerty_persistence_inevitability__strategic_lock_in_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_inevitability__strategic_lock_in_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qwerty_persistence_inevitability__strategic_lock_in_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qwerty_persistence_inevitability__strategic_lock_in_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.58: substantial but not confiscatory, because the arrangement performed real coordinating work alongside the margin protection, and portability of skill genuinely helped typists' employability even as the layout's costs fell on them. Suppression is authored at 0.34 as a present-tense structural property: the enforcement machinery (cartel terms, exclusive school contracts, dealer agreements) ran hard from 1893 through the 1930s and has since decayed into defaults and procurement habit, which the suppression_requirement series tracks falling from a 0.64 peak to 0.34. Theater_ratio 0.35: the efficiency mythology around the layout, including the slowed-down-typist legend and the scientific-superiority claims deployed against Dvorak-era challengers, grew fastest exactly when the arrangement needed defending, peaking near 0.46 around 1933 before settling as the defense shifted from argument to inertia. Accessibility_collapse 0.55: rival layouts remained purchasable for decades, so alternatives never collapsed completely, but the practical route to adoption (schools plus dealers plus employer demand) closed almost entirely. Resistance 0.45: concentrated and articulate among excluded manufacturers and layout reformers, diffuse and largely unvoiced among typists. All three series share one time grid (0-140 at twenty-year steps) so no metric is sampled against another's gaps.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the trust members' position the standard is their own coordination achievement: they wrote the agreement, they could have unmade it, and the arrangement subsidized them, placing their directionality near the beneficiary pole with effective costs damped or inverted. From the typists' position the same loop operates as a layout they did not choose, cannot leave, and pay for daily in fingers and hours; trapped exit places them near the full-target pole with amplified effective costs. Schools and employers sit between: both collect real benefits, but neither controls the terms, and the schools' institutional identity fused with the standard curriculum (we teach the machine the offices use) would make even a willing defection commercially self-destructive. Same-power differentiation: employing offices and typing schools hold comparable organized power, but their exits differ, since an employer could in principle have specified any layout had a graduate pool existed, while a school's entire product was the standard curriculum itself.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations drive the derivation. union_typewriter_trust_members, commercial_typing_schools, and employing_offices are declared beneficiaries, so their directionality sits near the subsidy pole: lowest for the trust, which both sets the terms and collects, somewhat higher for schools and employers, who collect without controlling. professional_typists are declared victims with trapped exit, placing them nearest the full-target pole; non_qwerty_keyboard_manufacturers are victims with constrained exit, near but not at the target pole since joining the trust remained formally available. No directionality overrides are needed: the structural declarations plus exit options reproduce the intended relationships. The one candidate override, marking the schools closer to symmetric because their dependence on employer demand limited their autonomy, would refine a second-order effect the derivation already bounds.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem, layout fragmentation across competing machine makers, died with the mechanical-typewriter era while the arrangement persists; that is the classic mandate-outlived-function shape, and the dead-status genealogy combined with a world_rearranges disappearance verdict flags it as such. What keeps this from resolving as bare vestige is that the cost-bearing did not die with the mandate: ergonomic costs and retraining barriers remain attached to the layout, and a generalized coordination function (any-layout compatibility across devices and skills) remains genuinely load-bearing. The classification therefore has to hold both truths at once, a real coordination function that blocks a pure-extraction reading and an engineered, asymmetric cost structure that blocks a pure-coordination reading, which is exactly the tangled-rope structure. Mislabeling risks run both ways: reading the arrangement as pure coordination launders the cartel's margin protection as public-spirited standardization; reading it as pure extraction erases the portability benefits typists themselves drew from a common layout.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is the strategic_lock_in_reading of kernel qwerty_persistence_inevitability; what structurally changes if the path_dependency_reading is adopted instead?',
    'Author and compile the sibling file qwerty_persistence_inevitability__path_dependency_reading with an emptied beneficiary set and accident-driven persistence framing, then compare per-seat classifications and epsilon across the pair.',
    'Under the sibling reading the beneficiary set empties, epsilon falls toward coordination-floor levels, and the computed type shifts toward rope; the disagreement is located in intentionality and beneficiary structure, not chronology.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: one reading of a contested kernel; the sibling reading changes beneficiary structure and epsilon.').

omega_variable(
    cartel_intentionality_evidence,
    'Do the archival record and trade press establish that the 1893 standardization agreement was adopted to secure member margins and exclude rival layouts, rather than primarily to solve fragmentation with margin protection as a side effect?',
    'Corporate archives of the Union Typewriter constituent firms, the trade press of 1889-1894, and the merger-negotiation correspondence, read against the timing of the school partnership contracts.',
    'Strong intentionality evidence anchors this reading''s engineered-persistence premise; weak evidence collapses the reading toward the path-dependent sibling and lowers epsilon accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cartel_intentionality_evidence, empirical, 'Whether the standardization agreement was deliberately protective of member rents or incidentally so.').

omega_variable(
    ergonomic_cost_attribution,
    'How much of the cost burden attributed to the standard layout is specific to QWERTY''s finger-load pattern, and how much is generic to any mature input standard''s switching costs?',
    'Comparative ergonomics studies of alternative layouts under matched training and workload conditions, plus historical retraining-cost accounting from the Dvorak-era Navy and Civil Service trials.',
    'If most measured costs are generic to standardization as such, epsilon falls and this reading converges toward the sibling; if the costs are layout-specific, the engineered cost-transfer claim stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ergonomic_cost_attribution, empirical, 'Attribution of the typist cost burden between layout-specific design and generic switching costs.').

omega_variable(
    residual_enforcement_vs_inertia,
    'Is present-day persistence maintained by active enforcement (operating-system defaults, procurement specifications, school curricula) or by pure installed-base inertia?',
    'Counterfactual analysis of institutions and jurisdictions that mandated neutral layout selection: if alternative-layout adoption remains negligible where nothing enforces the standard, inertia dominates.',
    'If inertia dominates, the arrangement''s remaining coercive force is near zero and the structure drifts toward vestigial theatrical maintenance; if defaults and procurement actively foreclose alternatives, the enforcement leg of the tangled rope remains live.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_enforcement_vs_inertia, empirical, 'Whether the enforcement leg of the arrangement is still active or has decayed into inertia.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_inevitability__strategic_lock_in_reading, 0, 140).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t0, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(qwer_tr_t20, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement(qwer_tr_t40, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 40, 0.36).
narrative_ontology:measurement(qwer_tr_t60, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 60, 0.46).
narrative_ontology:measurement(qwer_tr_t80, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 80, 0.44).
narrative_ontology:measurement(qwer_tr_t100, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 100, 0.4).
narrative_ontology:measurement(qwer_tr_t120, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 120, 0.37).
narrative_ontology:measurement(qwer_tr_t140, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 140, 0.35).

% Extraction over time
narrative_ontology:measurement(qwer_be_t0, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(qwer_be_t20, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(qwer_be_t40, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 40, 0.64).
narrative_ontology:measurement(qwer_be_t60, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 60, 0.66).
narrative_ontology:measurement(qwer_be_t80, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 80, 0.63).
narrative_ontology:measurement(qwer_be_t100, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 100, 0.61).
narrative_ontology:measurement(qwer_be_t120, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 120, 0.59).
narrative_ontology:measurement(qwer_be_t140, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 140, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t0, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(qwer_su_t20, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(qwer_su_t40, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 40, 0.64).
narrative_ontology:measurement(qwer_su_t60, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 60, 0.6).
narrative_ontology:measurement(qwer_su_t80, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 80, 0.52).
narrative_ontology:measurement(qwer_su_t100, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 100, 0.44).
narrative_ontology:measurement(qwer_su_t120, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 120, 0.38).
narrative_ontology:measurement(qwer_su_t140, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 140, 0.34).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_inevitability__strategic_lock_in_reading, information_standard).
narrative_ontology:affects_constraint(qwerty_persistence_inevitability__strategic_lock_in_reading, qwerty_persistence_inevitability__path_dependency_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'QWERTY persistence' decomposes under the epsilon-invariance principle into two structurally distinct claims sharing one kernel. This file is the strategic_lock_in_reading (engineered persistence, populated beneficiary and victim sets, epsilon 0.58, claimed tangled_rope); the sibling qwerty_persistence_inevitability__path_dependency_reading is the accident-driven reading (empty beneficiary set, epsilon near coordination floor, claimed rope). The upstream/downstream asymmetry runs from the chronological record both share: whichever reading a scholar adopts governs how the same 1893 archival facts are weighted, so each file links the other to make the family explicit and prevent either epsilon from being read as THE epsilon of 'QWERTY persistence'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
