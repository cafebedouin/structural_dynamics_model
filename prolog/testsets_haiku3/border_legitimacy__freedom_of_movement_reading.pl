% ============================================================================
% CONSTRAINT STORY: border_legitimacy__freedom_of_movement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_legitimacy__freedom_of_movement_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: border_legitimacy__freedom_of_movement_reading
 *   human_readable: Border Restriction as Presumptively Illegitimate (Freedom of Movement Reading)
 *   domain: political_philosophy/migration_studies/international_law
 *
 * SUMMARY:
 *   Under the freedom-of-movement reading of the border-legitimacy kernel,
 *   borders are presumptively illegitimate restrictions on a human right. The
 *   constraint the reading describes is the border-enforcement system
 *   itself—the machinery that restricts movement. From this reading's
 *   perspective, economically displaced workers, aspiring migrants, and those
 *   fleeing danger are the primary victims; their extraction is denial of
 *   movement rights. The constraint is claimed as a snare because its
 *   persistence depends on active enforcement (deportations, visa denials,
 *   physical barriers) and its beneficiaries (high-wage labor incumbents,
 *   welfare-state citizens, border-administration apparatus) are
 *   identifiable. The reading's core claim—that freedom of movement is a
 *   fundamental human right and borders are therefore presumptively
 *   illegitimate—is distinct from the sovereignty reading (which grounds
 *   legitimacy in territorial control) and the humanitarian reading (which
 *   permits borders for economic migrants but requires openness to refugees).
 *   This is ONE reading of the contested kernel; the other readings
 *   instantiate different constraints with different ε values and
 *   victim/beneficiary sets. The committer structure (the kernel contest
 *   itself) is routed through omega variables, not embedded in the constraint
 *   classification.
 *
 * KEY AGENTS:
 *   - border_enforcement_apparatus: Institutional actor that administers and enforces restrictions; sits as agenda-setter
 *   - incumbent_national_electorate: Organized political coalition that votes for border maintenance; dual role as agenda-setter and beneficiary of closure
 *   - economically_displaced_workers: Powerless victims excluded from labor markets by borders; trapped by the constraint
 *   - aspiring_migrants: Powerless victims seeking movement rights denied by borders; core extraction targets under this reading
 *   - welfare_dependent_current_citizens: Ambiguous position—benefit from welfare-state closure but cast as complicit in extractive exclusion
 *   - high_wage_labor_market_incumbents: Hidden beneficiaries who collect labor-scarcity rents from border-protected exclusion
 *   - sovereignty_reading_advocates: Excluded voices representing the competing legitimacy framing
 *   - humanitarian_obligation_advocates: Excluded voices representing a compromise position
 *   - international_human_rights_bodies: Observer seat; adjudicate border restrictions under human rights law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_legitimacy__freedom_of_movement_reading, 0.82).
domain_priors:suppression_score(border_legitimacy__freedom_of_movement_reading, 0.76).
domain_priors:theater_ratio(border_legitimacy__freedom_of_movement_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_legitimacy__freedom_of_movement_reading, snare).
narrative_ontology:human_readable(border_legitimacy__freedom_of_movement_reading, "Border Restriction as Presumptively Illegitimate (Freedom of Movement Reading)").
narrative_ontology:topic_domain(border_legitimacy__freedom_of_movement_reading, "political_philosophy/migration_studies/international_law").

domain_priors:requires_active_enforcement(border_legitimacy__freedom_of_movement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_legitimacy__freedom_of_movement_reading, '20f6762b-35a9-4390-82ea-744ed415243d').
narrative_ontology:cs_kernel_codification('20f6762b-35a9-4390-82ea-744ed415243d', distributed).
narrative_ontology:cs_authority_grounding('20f6762b-35a9-4390-82ea-744ed415243d', distributed).
narrative_ontology:cs_reading_relation('20f6762b-35a9-4390-82ea-744ed415243d', border_legitimacy__sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('20f6762b-35a9-4390-82ea-744ed415243d', border_legitimacy__humanitarian_obligation_reading, coexists_with).
narrative_ontology:cs_axiom('20f6762b-35a9-4390-82ea-744ed415243d', foundational, freedom_of_movement_as_foundational_right).
narrative_ontology:cs_axiom_status(freedom_of_movement_as_foundational_right, holdable).
narrative_ontology:cs_axiom_grounding('20f6762b-35a9-4390-82ea-744ed415243d', freedom_of_movement_as_foundational_right, deontological).
narrative_ontology:cs_axiom('20f6762b-35a9-4390-82ea-744ed415243d', foundational, territorial_borders_presumptively_illegitimate).
narrative_ontology:cs_axiom_status(territorial_borders_presumptively_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('20f6762b-35a9-4390-82ea-744ed415243d', territorial_borders_presumptively_illegitimate, deontological).
narrative_ontology:cs_reference_frame('20f6762b-35a9-4390-82ea-744ed415243d', universal_human_mobility_right).
narrative_ontology:cs_drift_state('20f6762b-35a9-4390-82ea-744ed415243d', contemporary_border_enforcement_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('20f6762b-35a9-4390-82ea-744ed415243d', '').
narrative_ontology:cs_kernel_id(border_legitimacy__freedom_of_movement_reading, border_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, economically_displaced_workers).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, welfare_dependent_current_citizens).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, aspiring_migrants).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, refugee_adjacent_persons).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(border_legitimacy__freedom_of_movement_reading, incumbent_national_electorate).
narrative_ontology:constraint_beneficiary(border_legitimacy__freedom_of_movement_reading, high_wage_labor_market_incumbents).
narrative_ontology:constraint_vindicates(border_legitimacy__freedom_of_movement_reading, universal_human_mobility_right).
narrative_ontology:constraint_vindicates(border_legitimacy__freedom_of_movement_reading, anti_territorial_exclusion_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and enforces border restrictions: deploys physical infrastructure, immigration courts, deportation machinery, and administrative review. Justifies enforcement as protecting national order and resource distribution. Persists regardless of whether the foundational legitimacy of borders is contested—enforcement is decoupled from the normative question of whether restriction itself is justified.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, border_enforcement_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Holds formal voting authority in the state. Frames borders as protecting existing citizens' welfare distribution, labor-market access, and cultural cohesion. Under this reading, the incumbent electorate's protective impulse itself becomes a target of critique—they are cast as extracting privilege (labor scarcity rents, welfare state closure) from those shut out. Their exit options include emigration (mobile), but domestically they are positioned as the enforcing coalition.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, incumbent_national_electorate, agenda_setter,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(border_legitimacy__freedom_of_movement_reading, incumbent_national_electorate, beneficiary).

% Labor-displaced by automation, offshoring, or resource exhaustion in their origin region. Borders prevent them from seeking work where jobs exist. They bear the extraction of exclusion: unable to move to labor-scarce regions where their productivity would be higher, they remain trapped in low-wage or unemployment situations. Their structural position under this reading is victimhood—borders extract their potential by denying them arbitrage.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, economically_displaced_workers, payer,
    powerless, biographical, trapped, global).

% Receive means-tested public benefits in a welfare state. Under this reading, borders protect the fiscal closure that makes that welfare possible—by restricting who can claim benefits, the state keeps the benefit pool smaller and per-recipient support higher. This reading casts welfare-dependent citizens as collateral victims of the border system: they benefit from exclusion (higher per-capita welfare), but the constraint's fundamental injustice (denying movement rights to outsiders) implicates them as passive participants in extractive exclusion. Their moral position is uncomfortable: they are both payers (complicit in exclusion) and victims (trapped by the system that sustains them).
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, welfare_dependent_current_citizens, payer,
    powerless, biographical, constrained, national).

% Wish to move across borders for economic opportunity, family reunion, or safety but are barred by immigration law and enforcement. They bear the core extraction this reading identifies: denial of freedom of movement. They are trapped in origin situations where their options are constrained. Under this reading, they are the primary victims—their human right to move is denied, and no legitimate state interest justifies the restriction.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, aspiring_migrants, payer,
    powerless, biographical, trapped, global).

% Face danger (persecution, climate disaster, violence) but do not meet narrow legal definitions of refugee status. Borders bar them from safety. Under this reading, borders extract their safety by denying movement rights even in extremity. They sit at the intersection of moral clarity (the constraint harms them) and normative contestation (the humanitarian and sovereignty readings disagree on whether borders must yield here).
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, refugee_adjacent_persons, payer,
    powerless, immediate, trapped, global).

% Benefit from border-protected labor scarcity: by excluding lower-wage workers from abroad, they sustain wage premiums in high-income regions. Under this reading, they are the hidden beneficiaries of border extraction. Their situation is structurally opaque—borders are not typically framed as protecting their rents, but this reading's logic identifies them as collecting the extraction through artificially high wages. They have high exit options (can migrate to other high-wage markets or to lower-wage regions as executives/investors).
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, high_wage_labor_market_incumbents, beneficiary,
    powerful, biographical, arbitrage, global).

% Hold the competing sovereignty reading: borders are legitimate because states derive authority from territorial control and democratic self-determination. They are excluded from this constraint story's frame—they are not parties to the arrangement under contest (the border system), but rather advocates of an alternative legitimacy framing. Under this reading, they represent the foundational disagreement the kernel encodes.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, sovereignty_reading_advocates, excluded,
    organized, generational, analytical, global).

% Hold a middle reading: borders are legitimate for economic migrants but should yield for those fleeing persecution or disaster. They are excluded from this freedom-of-movement reading's frame—they represent a compromise position neither this reading nor the sovereignty reading endorses. Their absence from the conversation is where the kernel contest lives.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, humanitarian_obligation_advocates, excluded,
    organized, generational, analytical, global).

% Monitor and adjudicate border restrictions under human rights law. They see borders as prima facie restrictions on movement rights and demand justification. They sit in the observer seat—they take testimony from the other seats but do not set the agenda unilaterally (though they influence it through soft law and strategic litigation).
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_legitimacy__freedom_of_movement_reading, high_wage_labor_market_incumbents).
narrative_ontology:fixing_cost_class(border_legitimacy__freedom_of_movement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under this reading, borders do not solve a genuine coordination problem—they enforce exclusion. The stated coordination functions (national security, welfare state sustainability, cultural preservation) are reframed as cover stories for extraction. No real coordination justifies denying people the right to move.
% TRANSFER_FUNCTION: Transfers labor-scarcity rents from excluded workers and migrants to incumbent labor-market participants in high-wage regions, and transfers welfare-state closure benefits from excluded non-citizens to incumbent citizens of welfare states. The constraint moves opportunity and safety from those outside the border to those inside.
% ABSENT_VOICES: The primary excluded voice is the aspiring migrant and displaced worker: they would argue that borders violate their human right to move and that no legitimate state interest overrides that right. Under this reading, they are absent not by accident but by design—border enforcement machinery excludes them from the political conversation about border legitimacy itself. Sovereignty advocates are also absent from this frame—they hold a reading the freedom-of-movement reading does not endorse.
% DISAPPEARANCE_RATIONALE: If borders disappeared overnight and freedom of movement became operative, labor markets would rebalance (wage convergence between high and low-income regions), welfare states would face fiscal pressure requiring retooling or convergence, and population distributions would shift. The entire architecture of national welfare distribution, labor-market rents, and citizenship privilege would reorganize. The constraint is not a natural law—its disappearance would cause massive structural change precisely because it is a system of extraction.
% FOUNDING_PROBLEM: The founding problem, under this reading, is not a genuine coordination failure but rather the historical triumph of state sovereignty claims over human mobility rights. Borders were established to serve state revenue collection, military conscription, and labor control. The framing of borders as solving legitimate coordination problems is a retrospective legitimacy construction, not the constraint's origin.
% FOUNDING_PROBLEM_CORROBORATION: Historical scholarship on border emergence (Torpey on state monopoly on movement, Sassen on state formation and labor control) supports the reading that borders originated in state administrative needs, not in solving genuine coordination problems. This corroboration comes from outside the benefiting parties (border enforcers, incumbent electorates, labor-market incumbents). Sovereignty reading advocates dispute this and argue the founding problem is ongoing: maintaining stable political orders and welfare distributions requires border control. However, the reading's corroboration from historical scholarship is the external testimony cited here.
narrative_ontology:disappearance_verdict(border_legitimacy__freedom_of_movement_reading, world_rearranges).
narrative_ontology:founding_problem_status(border_legitimacy__freedom_of_movement_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_legitimacy__freedom_of_movement_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(border_legitimacy__freedom_of_movement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(border_legitimacy__freedom_of_movement_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_legitimacy__freedom_of_movement_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_legitimacy__freedom_of_movement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_legitimacy__freedom_of_movement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because under this reading borders deny a fundamental right (movement) without compensating benefit to those denied. The extraction is not contingent on market conditions—it is structural exclusion. Suppression is substantial (0.76) because the constraint's persistence depends on active enforcement: border patrol, visa controls, deportation machinery, and legal regimes that criminalize unauthorized crossing. The enforcement is decoupled from solving any real coordination problem—it purely enforces exclusion. Theater is moderate (0.41): some border functions are genuine (disease screening, security vetting), but a significant share of enforcement activity defends pure exclusion rather than addressing legitimate public concerns. Accessibility collapse is moderate-to-high (0.68) because alternatives to the border system exist conceptually (open migration, freedom of movement), but once the constraint is accepted as legitimate, people internalize its inevitability and exit options collapse. Resistance is high (0.71) because the reading is actively contested by sovereignty advocates and faces organized resistance from incumbent electorates—the constraint is not quietly accepted. The measurement series shows extractiveness rising over the interval (0.71 to 0.82) as enforcement infrastructure intensifies and border barriers become more sophisticated (physical walls, biometric systems, visa complexity). Suppression plateaus at 0.76 because enforcement reaches a steady-state intensity—the machinery is fully constructed by mid-interval. Theater rises gradually (0.32 to 0.41) as enforcement increasingly takes on justificatory discourse (security narratives, humanitarian exceptions) to legitimize pure exclusion.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute radically different classifications depending on the seat: from the border-enforcement apparatus's position, the constraint may compute as rope (coordination of security, orderly distribution) or tangled rope (genuine coordination plus extraction); from the aspiring migrant's position, it computes as pure snare (no coordination benefit, only denial). From the incumbent electorate's position, it computes as rope (coordination of welfare distribution and labor-market protection). The same structural data produces different per-seat types because directionality differs—beneficiaries compute the constraint as less extractive, targets compute it as highly extractive. This perspectival divergence IS the measurement the framework exists to make. The reading's core claim (freedom of movement is a right; borders are presumptively illegitimate) is not a metric—it is an axiom. The metrics describe what the constraint does, not whether it is justified.
 *
 * DIRECTIONALITY LOGIC:
 *   Victims (displaced workers, aspiring migrants, refugee-adjacent persons) are trapped or identity-locked by the constraint—they cannot exit the status of 'excluded' except through illegal/dangerous means. Their directionality sits at the target end (d near 1.0). The hidden beneficiaries (high-wage labor incumbents) benefit from scarcity rents; their directionality is near the beneficiary end (d near 0.0), though the benefit is not publicly named or acknowledged—they are not the official agenda-setters, which is why the extraction is hidden. The border-enforcement apparatus is the agenda-setter (administers and enforces), making its d moderately toward the collector end, though it is instrumental rather than the primary beneficiary. The incumbent electorate's position is complex: they formally set the agenda (vote for borders) and benefit from welfare-closure and labor-scarcity effects, but under this reading they are also cast as collateral beneficiaries of injustice, which could place them in an ambiguous middle position (d near 0.5). No directionality overrides are needed—the structural data (beneficiaries/victims, power levels, exit options) drive the derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (state administrative needs, labor control, revenue extraction in early state formation) is NOT the problem borders are now justified as solving (security, welfare distribution, cultural preservation). This reading diagnoses mandatrophy: the constraint persists despite the founding problem being obsolete or not the real reason for its continuation. Borders were never really about solving genuine coordination problems; they were about state power. Contemporary justifications (security, welfare, culture) are post-hoc legitimacy constructions, not explanations of origin. However, mandatrophy is contestable here because the sovereignty reading argues that border maintenance IS continuously solving a real problem: sustaining political orders and democratic self-determination. The mandatrophy resolution depends on whether you accept the reading's core premise.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_foreclosure_or_coexistence,
    'Does the freedom-of-movement reading logically foreclose the sovereignty reading, or do both readings remain live positions held by different parties?',
    'Examine whether a single framework (constitutional, ethical, institutional) could coherently hold both: if borders are a human right violation, can a state also derive legitimate authority from territorial control? If yes, the readings coexist (different parties, different frameworks); if no, the reading forecloses sovereignty.',
    'If foreclosure holds, this reading is the truth and sovereignty advocates are simply mistaken. If coexistence holds, the kernel contest is genuine and no reading eliminates its competitors. The cs_structure.reading_relations field asserts coexistence (both readings live), but this omega flags the ambiguity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_or_coexistence, conceptual, 'Whether this reading''s core premise logically eliminates or merely contradicts the sovereignty reading.').

omega_variable(
    extraction_hidden_vs_overt,
    'Is the extraction of labor-scarcity rents by high-wage incumbents a hidden feature of border operation, or is it incidental to the genuine coordination problems borders solve?',
    'Compare labor-market and wage outcomes in open-border vs. closed-border contexts; examine whether high-wage incumbents actively lobby for border maintenance; trace economic benefit flows to organized constituencies.',
    'If extraction is central and organized, the snare classification is robust. If extraction is incidental, the constraint may compute as rope or tangled rope depending on the seat. If high-wage incumbents are not actually the beneficiaries (if benefits are more diffuse), the victim set may need revision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_hidden_vs_overt, empirical, 'Whether border-protected labor scarcity is a primary extractive mechanism or incidental to other functions.').

omega_variable(
    welfare_citizen_complicity_ambiguity,
    'Are welfare-dependent current citizens beneficiaries or victims of border extraction?',
    'Model welfare-state fiscal closure under open borders: if per-capita benefits would decline, welfare citizens are currently beneficiaries of closure; if fiscal restructuring could maintain or expand benefits without closure, they are victims of a false choice imposed by incumbent electorates.',
    'If welfare citizens are genuine beneficiaries, they should move from the victim set to a dual-positioned beneficiary role. If they are victims of a false choice, they should remain in victims. This affects the directionality derivation for the organized-power organized-stakeholder seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_citizen_complicity_ambiguity, empirical, 'Whether welfare-state closure is necessary or contingent on incumbent preferences.').

omega_variable(
    freedom_of_movement_as_foundational_right,
    'Is freedom of movement a foundational human right that overrides state sovereignty claims, or is it a contingent right that can be legitimately restricted by political communities?',
    'This is not resolvable by data—it is a normative commitment. The resolution is to recognize this as the core dividing axiom between this reading and the sovereignty reading. Different moral frameworks (cosmopolitanism vs. communitarianism, natural rights vs. contractarian) will produce different answers.',
    'If movement is foundational, borders are presumptively illegitimate and ε remains high. If movement is contingent, borders can be legitimate and ε drops. The engine does not resolve this—it measures the structural consequences of each reading''s assumption.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(freedom_of_movement_as_foundational_right, preference, 'The foundational normative commitment of this reading versus its competitors.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression (0.76) structural (external enforcement machinery, legal barriers, physical walls) or internalized (people have absorbed border legitimacy and police their own exit aspirations)?',
    'Post-exit observation: if migrants who successfully cross borders report persistence of internalized exit barriers (self-doubt, identity-fusion with origin), suppression is partially internalized. If aspiring migrants report that removal of external barriers alone enables movement, suppression is primarily structural.',
    'If internalized, the constraint''s effective suppression may be higher than the measured value—the target carries the suppression with them. If structural, removing the enforcement machinery would dissolve suppression quickly. This affects piton vs. snare classification: high internalization suggests institutional inertia, while pure structure suggests active extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether suppression of movement aspiration is maintained by external barriers or internalized identity-fusion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_legitimacy__freedom_of_movement_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_legitimacy__freedom_of_movement_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement_basis(bord_tr_t0, observed).
narrative_ontology:measurement(bord_tr_t5, border_legitimacy__freedom_of_movement_reading, theater_ratio, 5, 0.35).
narrative_ontology:measurement_basis(bord_tr_t5, observed).
narrative_ontology:measurement(bord_tr_t10, border_legitimacy__freedom_of_movement_reading, theater_ratio, 10, 0.37).
narrative_ontology:measurement_basis(bord_tr_t10, observed).
narrative_ontology:measurement(bord_tr_t15, border_legitimacy__freedom_of_movement_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement_basis(bord_tr_t15, observed).
narrative_ontology:measurement(bord_tr_t20, border_legitimacy__freedom_of_movement_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(bord_tr_t20, observed).
narrative_ontology:measurement(bord_tr_t25, border_legitimacy__freedom_of_movement_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(bord_tr_t25, observed).
narrative_ontology:measurement(bord_tr_t30, border_legitimacy__freedom_of_movement_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(bord_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 0, 0.71).
narrative_ontology:measurement_basis(bord_be_t0, observed).
narrative_ontology:measurement(bord_be_t5, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 5, 0.74).
narrative_ontology:measurement_basis(bord_be_t5, observed).
narrative_ontology:measurement(bord_be_t10, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 10, 0.77).
narrative_ontology:measurement_basis(bord_be_t10, observed).
narrative_ontology:measurement(bord_be_t15, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 15, 0.79).
narrative_ontology:measurement_basis(bord_be_t15, observed).
narrative_ontology:measurement(bord_be_t20, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 20, 0.81).
narrative_ontology:measurement_basis(bord_be_t20, observed).
narrative_ontology:measurement(bord_be_t25, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 25, 0.82).
narrative_ontology:measurement_basis(bord_be_t25, observed).
narrative_ontology:measurement(bord_be_t30, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 30, 0.82).
narrative_ontology:measurement_basis(bord_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement_basis(bord_su_t0, observed).
narrative_ontology:measurement(bord_su_t5, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 5, 0.7).
narrative_ontology:measurement_basis(bord_su_t5, observed).
narrative_ontology:measurement(bord_su_t10, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 10, 0.72).
narrative_ontology:measurement_basis(bord_su_t10, observed).
narrative_ontology:measurement(bord_su_t15, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 15, 0.73).
narrative_ontology:measurement_basis(bord_su_t15, observed).
narrative_ontology:measurement(bord_su_t20, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement_basis(bord_su_t20, observed).
narrative_ontology:measurement(bord_su_t25, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 25, 0.76).
narrative_ontology:measurement_basis(bord_su_t25, observed).
narrative_ontology:measurement(bord_su_t30, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 30, 0.76).
narrative_ontology:measurement_basis(bord_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_legitimacy__freedom_of_movement_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(border_legitimacy__freedom_of_movement_reading, 0.12).
narrative_ontology:affects_constraint(border_legitimacy__freedom_of_movement_reading, border_legitimacy__sovereignty_reading).
narrative_ontology:affects_constraint(border_legitimacy__freedom_of_movement_reading, border_legitimacy__humanitarian_obligation_reading).

% DUAL FORMULATION NOTE:
% The border-legitimacy kernel decomposes into three structurally distinct constraints, one per reading. This constraint (freedom-of-movement reading) instantiates the reading that treats borders as presumptively illegitimate restrictions on a human right, making the constraint's ε high and its victims the globally mobile populations excluded by borders. The sovereignty reading (separate file) treats borders as legitimate expressions of territorial authority, placing ε lower and reframing victims as those harmed by uncontrolled movement. The humanitarian reading (separate file) occupies a middle position. These are NOT different measurements of the same constraint—they are different constraints instantiated by different readings of the same contested kernel. Each has its own ε-invariance, its own beneficiary/victim structure, and its own classification. The ε-invariance principle requires separation: a single constraint file attempting to measure border legitimacy across readings would conflate the readings' ε values and produce incoherent results. Link via network.affects_constraints to enable cross-reading analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
