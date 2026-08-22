% ============================================================================
% CONSTRAINT STORY: tordesillas_demarcation_kernel__portuguese_exploration_legitimation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tordesillas_demarcation_kernel__portuguese_exploration_legitimation, []).

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
 *   constraint_id: tordesillas_demarcation_kernel__portuguese_exploration_legitimation
 *   human_readable: Tordesillas Demarcation - Portuguese Exploration-Legitimation Reading
 *   domain: international_law/colonial_history/sovereignty_theory
 *
 * SUMMARY:
 *   Two crusading crowns converged on the same newly reachable world. The
 *   bulls of 1493 and the Treaty of Tordesillas of 1494 drew a meridian 370
 *   leagues west of the Cape Verde islands: east of it, the Portuguese crown;
 *   west of it, Castile. This story instantiates ONE reading of that
 *   demarcation kernel - the Portuguese exploration-legitimation reading,
 *   under which the papal-treaty complex confirms rights already earned by a
 *   decade of sponsored voyages (Dias rounding Africa in 1488, Gama reaching
 *   India in 1498) and lawfully bars European rivals from the African-Indian
 *   Ocean sphere. Per the epsilon-invariance discipline, epsilon's referent
 *   is the standing eastern-sphere allocation as this reading frames it - not
 *   the sibling conquest reading's western referent, and not the outcome any
 *   seat would prefer. The instrument coordinated a real Iberian war-risk
 *   away while transferring the entire eastern trade option from every other
 *   European claimant to one crown: a genuine coordination function and
 *   asymmetric extraction operating through the same signature. The
 *   arrangement peaked as a functioning monopoly in the early sixteenth
 *   century, then decayed as northern powers defied it, until by interval end
 *   it survived mainly as ceremony, patronage right, and map legend. KEY
 *   AGENTS (by structural relationship): - portuguese_crown: Primary
 *   beneficiary and co-agenda-setter (institutional/arbitrage) - receives the
 *   eastern trade monopoly its explorers' voyages anchor - castilian_crown:
 *   Signatory turned target under this reading (institutional/constrained) -
 *   barred from the eastern route its own expedition set out to find -
 *   estado_da_india_merchants: Licensed beneficiaries (organized/constrained)
 *   - collect monopoly-margin returns on the annual carreira fleets -
 *   northern_european_interlopers: Excluded rivals and eventual defiers
 *   (organized/mobile) - bear the bar, then break it -
 *   italian_merchant_houses: Collateral payers (organized/constrained) - lose
 *   the Levant spicing intermediation the Cape route bypasses - papacy:
 *   Certifying agenda-setter (institutional/constrained) - lends
 *   jurisdictional authority, collects doctrinal rather than material return
 *   - indian_ocean_port_polities: Excluded voices (organized/trapped) - the
 *   allocated parties with no seat at the allocation -
 *   salamanca_school_jurists: Analytical observers (moderate/analytical) -
 *   attack the title theory beneath both Iberian readings
 *
 * KEY AGENTS:
 *   - portuguese_crown: primary beneficiary and co-agenda-setter (institutional/arbitrage)
 *   - castilian_crown: signatory turned target under this reading (institutional/constrained)
 *   - estado_da_india_merchants: licensed beneficiaries (organized/constrained)
 *   - northern_european_interlopers: excluded rivals and eventual defiers (organized/mobile)
 *   - italian_merchant_houses: collateral payers (organized/constrained)
 *   - papacy: certifying agenda-setter (institutional/constrained)
 *   - indian_ocean_port_polities: excluded voices (organized/trapped)
 *   - salamanca_school_jurists: analytical observers (moderate/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.42).
domain_priors:suppression_score(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.3).
domain_priors:theater_ratio(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, extractiveness, 0.42).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, tangled_rope).
narrative_ontology:human_readable(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, "Tordesillas Demarcation - Portuguese Exploration-Legitimation Reading").
narrative_ontology:topic_domain(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, "international_law/colonial_history/sovereignty_theory").

domain_priors:requires_active_enforcement(tordesillas_demarcation_kernel__portuguese_exploration_legitimation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, '41697878-5ddc-4bd8-8bad-1c98e5594885').
narrative_ontology:cs_kernel_codification('41697878-5ddc-4bd8-8bad-1c98e5594885', fixed_text).
narrative_ontology:cs_authority_grounding('41697878-5ddc-4bd8-8bad-1c98e5594885', lineage).
narrative_ontology:cs_interpretation_layer_present('41697878-5ddc-4bd8-8bad-1c98e5594885').
narrative_ontology:cs_reading_relation('41697878-5ddc-4bd8-8bad-1c98e5594885', tordesillas_demarcation_kernel__spanish_conquest_legitimation, coexists_with).
narrative_ontology:cs_axiom('41697878-5ddc-4bd8-8bad-1c98e5594885', foundational, exploration_confers_confirmable_title).
narrative_ontology:cs_axiom_status(exploration_confers_confirmable_title, holdable).
narrative_ontology:cs_axiom_grounding('41697878-5ddc-4bd8-8bad-1c98e5594885', exploration_confers_confirmable_title, conventional).
narrative_ontology:cs_axiom('41697878-5ddc-4bd8-8bad-1c98e5594885', secondary, first_navigator_earns_exclusive_route).
narrative_ontology:cs_axiom_status(first_navigator_earns_exclusive_route, holdable).
narrative_ontology:cs_axiom_grounding('41697878-5ddc-4bd8-8bad-1c98e5594885', first_navigator_earns_exclusive_route, instrumental).
narrative_ontology:cs_reference_frame('41697878-5ddc-4bd8-8bad-1c98e5594885', papal_confirmation_of_acquired_rights).
narrative_ontology:cs_drift_state('41697878-5ddc-4bd8-8bad-1c98e5594885', post_westphalian_maritime_order, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('41697878-5ddc-4bd8-8bad-1c98e5594885', '').
narrative_ontology:cs_kernel_id(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, tordesillas_demarcation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_crown).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, estado_da_india_merchants).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, castilian_crown).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, northern_european_interlopers).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, italian_merchant_houses).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, castilian_crown).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Co-negotiated and ratified the 1494 partition and administers everything east of the agreed meridian: licenses the Cape-route carreira fleets, taxes the spice cargoes at Lisbon, commissions the fortresses from Guinea to Malacca, and prosecutes the diplomatic defense of its exclusive claims. Its treasury receives the customs duties, contract fees, and royal-fifth shares the arrangement generates. Renegotiation remains available to it - demonstrated at Zaragoza in 1529, when it sold and bought adjustments to the antimeridian line.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_crown, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_crown, beneficiary).

% Signed the same instrument and holds the mirror-right west of the line, but its original object - an ocean route to the spice sources Columbus sailed to find - lies east of the line, where the treaty bars it. It finances westward ventures instead, litigates line placements at the Badajoz junta, and ultimately builds its empire on silver and land rather than spices. Repudiating the partition would expose its own western title, which rests on the same papal instruments.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, castilian_crown, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, castilian_crown, beneficiary).

% Hold crown licenses and contracts to ship pepper, cinnamon, and silk on the annual Goa-Lisbon carreira. The exclusion of rival European shipping keeps freight rates, purchase prices at source, and Lisbon auction returns favorable to them. Their capital is committed to the route's forts, factors, and convoy schedule, so their fortunes rise and fall with the monopoly's reach.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, estado_da_india_merchants, beneficiary,
    organized, biographical, constrained, global).

% English, French, and Dutch voyagers and, later, chartered companies arrive in the eastern seas from the mid-sixteenth century onward carrying no license the Portuguese crown recognizes. Early expeditions risk seizure; over time they sail in strength, break the monopoly at its weakest points, and build their own factories and routes. Their growing freedom of action comes from defying the arrangement rather than from any provision inside it.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, northern_european_interlopers, payer,
    organized, biographical, mobile, global).

% Venetian and Genoese firms had grown wealthy buying Eastern spices from Mamluk and Ottoman middlemen in Alexandria, Beirut, and Aleppo and retailing them across Europe. The Cape route delivers the same goods to Lisbon without passing their counting houses, and their volumes and margins fall accordingly. Their capital is sunk in Levant partnerships that the new route strands.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, italian_merchant_houses, payer,
    organized, generational, constrained, continental).

% Issued the bulls of 1493 and lent its jurisdictional authority to the 1494 instrument, adjudicating between the two crowns as vicar of a universal Christendom. It collects little material revenue from the arrangement; its return is doctrinal - each successful application reaffirms that the Holy See may allocate relations with non-Christian worlds. After the Reformation and the Westphalian settlements, fewer and fewer parties accept that it holds any such competence, but its own teaching never formally renounces the claim.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, papacy, agenda_setter,
    institutional, civilizational, constrained, universal).

% Rulers and merchant communities of Calicut, Hormuz, Malacca, Gujarat, and beyond, whose harbors, pilots, and markets the line presumes to allocate among distant kings. None was consulted, none signed, and none could appeal the allocation anywhere. Their first notice of the arrangement typically came with a Portuguese fleet anchoring off their roads and demanding terms.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, indian_ocean_port_polities, excluded,
    organized, generational, trapped, continental).

% Theologians and canon lawyers, Francisco de Vitoria foremost, examine the titles claimed under the papal instruments and argue that the pope holds no temporal power over infidel peoples and that discovery alone confers no right of dominion. They publish, lecture, and correspond; they command no fleet and move no line, but their arguments circulate through every chancery in Europe and arm later challengers of the whole framework.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, salamanca_school_jurists, observer,
    moderate, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_crown).
narrative_ontology:fixing_cost_class(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Settled, without war between the two Iberian crowns, the allocation of newly reachable ocean space and trade routes: each crown received an exclusive sphere in which it could invest in exploration, fortification, and commerce without fighting the other. The line converted a looming Iberian naval conflict into a negotiated partition, twice renewed by purchase and agreement rather than battle.
% TRANSFER_FUNCTION: Moves exclusive trading rights over the entire eastern hemisphere - the African and Indian Ocean sphere - from all other European claimants to the Portuguese crown, and symmetrically the western sphere to Castile, converting private exploration expenditure into legally recognized monopoly rent collected at Lisbon.
% ABSENT_VOICES: Every polity whose waters and markets the line bisected - Mamluk Egypt, Gujarat, Calicut, Hormuz, Malacca, Ming China, Japan - had no seat, nor did the African coastal societies, nor any non-signatory European power, which learned of the allocation as a fait accompli. Venice, whose Levant spicing the Cape route would destroy, protested through diplomatic channels but stood outside the settlement. Unanimity among the signatories arose partly because the parties who would have objected were never in the room.
% DISAPPEARANCE_RATIONALE: If the demarcation and its papal warrant vanished overnight, Iberian rivalry over the same discoveries likely turns to open war in the 1490s; the Estado da India's legal architecture of exclusive licenses loses its warrant; and the very extent of Brazil - Portuguese-speaking, on the line's eastern side - along with the later Luso-Brazilian border treaties that invoked the line, would be redrawn. Colonial claims, missionary patronage rights, and the map of two hemispheres all hang on the meridian.
% FOUNDING_PROBLEM: Overlapping Iberian claims to the same newly found lands and routes: two crusading crowns, each armed with papal grants, converging on the same Atlantic discoveries with no agreed rule to divide them, and the realistic alternative was war between the two leading Catholic powers.
% FOUNDING_PROBLEM_CORROBORATION: That the war-avoidance problem was real at signing is attested outside the benefiting parties by contemporary Italian diplomatic dispatches reporting genuine fear of an Iberian rupture, alongside the crowns' own negotiating record. That the problem died while the arrangement persisted is attested by Francis I's public protests that the partition lacked any lawful basis, by Grotius's Mare Liberum (1609) arguing the exclusion rested on no valid title, and by Dutch and English charter practice treating Portuguese claims as unenforceable. No source outside the Iberian-papal beneficiary set attests that the founding problem remained live past the mid-sixteenth century.
narrative_ontology:disappearance_verdict(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, world_rearranges).
narrative_ontology:founding_problem_status(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 'none', 1).
narrative_ontology:epsilon_provenance(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.42, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tordesillas_demarcation_kernel__portuguese_exploration_legitimation_tests).
:- end_tests(tordesillas_demarcation_kernel__portuguese_exploration_legitimation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The scalar base_properties values report the terminal (1700) state of the interval, matching the final column of the shared measurement grid; the series show the full lifecycle. Extractiveness peaks at 0.63 around 1580, when the Cape-route monopoly is fully operative and carreira profits flow, then decays to 0.42 as Dutch, English, and French shipping breaks the exclusion - the terminal figure is moderate, consistent with this reading's expected profile. Suppression_requirement is authored as a series because this story specifically tracks enforcement-capacity change: coercive machinery built up to roughly 1529 (seizure of interlopers, fortress network, papal backing at full credibility) and then attrited as defiance normalized and enforcement reach shrank. Theater_ratio crosses 0.5 after 1648: by then the line survives chiefly as juridical performance, map decoration, and patronage invocation rather than as an operative bar. Accessibility_collapse is low (0.25) because the constraint never closed the practical alternative of simply sailing - it collapsed the space of legitimate recognition, not physical access; resistance is high (0.70) because open defiance became the norm, from Francis I's protests through Grotius to the Dutch seizure of Portuguese posts. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and spatial scope, in the engine's computation. All three series run on one shared six-point grid so no metric row borrows another's end-state values.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently because the same signature does different things to each. From the Portuguese seat the instrument reads as earned confirmation: a decade of royal-financed risk-taking, ratified - the arrangement looks like the reward side of a bargain. From the Castilian seat the same instrument is a bar across the prize its own venture opened: Columbus sailed west to reach the spice sources and found them allocated to his employer's neighbor. From the northern seats it is a paper wall with a shrinking fleet behind it - illegitimate on its face, since they never consented to it. Same-level dynamics sharpen the divergence: Castile and Portugal hold identical institutional standing, yet experience opposite directionalities, differentiated not by power but by constraint-specific position - who had sunk exploration costs east of the line and who west of it. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   The Portuguese crown and the licensed carreira merchants sit near the beneficiary end: the arrangement subsidizes them, and the crown additionally authors and enforces it. The Castilian crown sits near the target end despite being a signatory - under this reading's referent it pays in the currency of foregone eastern access, and its exit is constrained because repudiation would expose its own western title resting on the same instruments. Northern interlopers are targets whose mobility damps their effective burden over time: their exit is precisely defiant sailing, which grows cheaper as their strength grows. Italian merchant houses are targets by competitive displacement - their centuries-old intermediary margins were transferred to Lisbon without their consent. The papacy sits near symmetric: it certifies and adjudicates but collects little material return; its stake is doctrinal vindication, which is recorded under vindicated_propositions (papal_temporal_authority_over_infidel_waters, exploration_confers_confirmable_title) rather than beneficiaries, because a vindicated doctrine collects no rents. The Indian Ocean polities are excluded rather than targeted by this reading's referent - the allocation presumed to dispose of their harbors without their presence, which is recorded as absence, not as authored victimhood.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - preventing war between the two leading Catholic powers over overlapping Atlantic claims - was real and live at signing, and the arrangement genuinely solved it: no Iberian-Iberian naval war over the Indies occurred, and the partition was twice renegotiated peacefully (Tordesillas itself improving on Alcacovas; Zaragoza in 1529 settling the antimeridian for cash). By mid-century the founding problem was dead: the partition was complete, and the 1580 union of the crowns mooted the line between the signatories altogether. Yet the arrangement persisted - padraodo patronage rights, ceremonial invocations, map legends - long after its coordinating work ended. The classification prevents mislabeling in both directions: calling the whole lifecycle pure extraction erases the real war-avoidance coordination that justified the instrument at birth; calling it pure coordination erases the exclusionary transfer that defined its operation. The honest structure is a tangled rope whose mandate expired while the form endured - mandatrophy_resolved is declared, the theater_ratio series marks the crossover into proxy-maintenance after 1648, and the terminal_inertia_vs_capture omega leaves the terminal phase's exact character (inertial versus still-captured) open for the engine and future measurement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is the portuguese_exploration_legitimation reading of tordesillas_demarcation_kernel; what structural facts would change if the sibling reading (spanish_conquest_legitimation) were the referent instead?',
    'Author and compare the sibling story: its victim set centers indigenous populations of the western hemisphere and its transfer runs through land seizure and labor control rather than trade exclusion.',
    'Under the sibling reading the same papal-treaty instruments carry a far higher epsilon and a different victim set; cross-reading comparison isolates what the demarcation kernel itself contributes versus what each reading adds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: this story is one of two readings of the Tordesillas demarcation kernel.').

omega_variable(
    confirmation_vs_concession,
    'Does the papal instrument confirm title acquired through exploration and occupation, or does it confer dominion that would not otherwise exist?',
    'Juridical-textual analysis of Inter caetera and Dudum siquidem against canon-law theories of discovery title, together with how the crowns themselves pleaded the grants in negotiation (the Badajoz junta arguments of 1524).',
    'The confirmation reading supports the moderate-epsilon profile (a reward for sunk exploration cost); the concession reading makes the grant an uncompensated gift of other peoples'' reachable worlds and pushes epsilon upward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(confirmation_vs_concession, conceptual, 'Where the two readings of the kernel actually disagree: the operative effect of the papal act.').

omega_variable(
    downstream_enforcement_boundary,
    'Does the Indian Ocean enforcement regime (cartaz passports, fortress tolls, seizures of Asian shipping) belong inside this constraint''s victim set, or is it a distinct downstream constraint?',
    'Epsilon-invariance test: measure exclusion-of-European-rivals and coercion-of-Asian-commerce as separate observables; if epsilon differs across them, decompose into a linked family member (a cartaz-system story).',
    'Folding Asian maritime communities into the victim set raises epsilon well above moderate and shifts the profile toward pure extraction; holding the boundary preserves this story''s European-allocation referent and its tangled-rope shape.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(downstream_enforcement_boundary, conceptual, 'Victim-set boundary between the European allocation layer and the Asian enforcement layer.').

omega_variable(
    terminal_inertia_vs_capture,
    'After 1648, is the persisting arrangement inertial residue (ceremonial padraodo claims few could collect) or still-captured rent (patronage revenues, licensing remnants)?',
    'Trace Portuguese royal receipts attributable to padraodo ecclesiastical patronage and eastern licensing, and count serious diplomatic invocations of the line, 1648-1700.',
    'Pure inertia supports piton-direction drift at the terminal phase; continuing concentrated receipts support persistent capture and a snare-flavored tail.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(terminal_inertia_vs_capture, empirical, 'Whether the arrangement''s late-interval persistence is theatrical inertia or ongoing capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 1494, 1700).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tord_tr_t1494, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1494, 0.12).
narrative_ontology:measurement(tord_tr_t1529, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1529, 0.18).
narrative_ontology:measurement(tord_tr_t1580, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1580, 0.28).
narrative_ontology:measurement(tord_tr_t1609, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1609, 0.38).
narrative_ontology:measurement(tord_tr_t1648, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1648, 0.5).
narrative_ontology:measurement(tord_tr_t1700, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1700, 0.6).

% Extraction over time
narrative_ontology:measurement(tord_be_t1494, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1494, 0.35).
narrative_ontology:measurement(tord_be_t1529, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1529, 0.6).
narrative_ontology:measurement(tord_be_t1580, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1580, 0.63).
narrative_ontology:measurement(tord_be_t1609, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1609, 0.56).
narrative_ontology:measurement(tord_be_t1648, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1648, 0.48).
narrative_ontology:measurement(tord_be_t1700, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1700, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(tord_su_t1494, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1494, 0.55).
narrative_ontology:measurement(tord_su_t1529, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1529, 0.62).
narrative_ontology:measurement(tord_su_t1580, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1580, 0.58).
narrative_ontology:measurement(tord_su_t1609, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1609, 0.48).
narrative_ontology:measurement(tord_su_t1648, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1648, 0.38).
narrative_ontology:measurement(tord_su_t1700, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1700, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, resource_allocation).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, tordesillas_demarcation_kernel__spanish_conquest_legitimation).

% DUAL FORMULATION NOTE:
% The colloquial label 'Treaty of Tordesillas' conflates two structurally distinct constraints sharing one kernel. This story authors the eastern-sphere exploration-legitimation allocation: moderate epsilon, victim set drawn from rival European powers, transfer operating through trade-monopoly exclusion. The sibling story authors the western-sphere conquest license: substantially higher epsilon, victim set centered on indigenous populations, transfer operating through land and labor seizure. Each file carries its own epsilon, beneficiaries, and victims per the epsilon-invariance principle, and the stories link through network.affects_constraints. A prospective third family member - the Estado da India's cartaz enforcement regime, which would carry the Asian-merchant victim set this reading deliberately excludes - is flagged in the downstream_enforcement_boundary omega rather than invented here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
