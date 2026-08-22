% ============================================================================
% CONSTRAINT STORY: reformation_composite__technological_mediation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_composite__technological_mediation_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: reformation_composite__technological_mediation_reading
 *   human_readable: Print Mediation of Religious Dissent, 1517-1555 (Technological Mediation Reading)
 *   domain: historical epistemology/religious history/political economy
 *
 * SUMMARY:
 *   This story instantiates the technological_mediation_reading of the
 *   reformation_composite kernel: the claim that the Reformation became a
 *   continental mass movement because the printing press transformed the
 *   conditions under which religious dissent could scale. The constraint
 *   under classification is the print-mediation channel of 1517-1555 — the
 *   structural fact that mass, standardized, repeatable text reproduction was
 *   available only through the press, whose physical and economic properties
 *   (fixed composition costs, near-zero marginal reproduction, format
 *   selection, permanence of the printed word, literacy requirement) fixed
 *   the form and scale of the movement. Per the kernel-reading discipline,
 *   this is ONE reading generated as a clean epsilon-invariant constraint:
 *   the epsilon referent is the print-mediated communication arrangement
 *   itself, assessed by this reading's own lights as enabling infrastructure,
 *   never the theological or political arrangements the sibling readings
 *   contest. The claim/metric relationship is authored independently:
 *   claimed_type is mountain (the reading's own framing — a physical
 *   constraint enabling all other dimensions) while the declared
 *   beneficiaries, victims, and parties let the engine compute per-seat
 *   classifications and any false-summit signature without reference to the
 *   claim.
 *
 * KEY AGENTS:
 *   - printers_publishers: primary beneficiary and channel operator (organized/mobile) — collects revenue from every side's output; holds the practical gate on what is set in type
 *   - vernacular_reformist_authors: secondary beneficiary (moderate/identity_locked) — gains the mass channel; pays in permanent public exposure
 *   - literate_urban_publics: beneficiary-purchasers (moderate/mobile) — gain unmediated access to controversy; pay book prices
 *   - papal_imperial_church: net payer with incidental beneficiary position (institutional/trapped) — loses informational control; remains the channel's largest customer
 *   - manuscript_culture_practitioners: displaced payers (powerless/identity_locked) — bear the channel's displacement cost
 *   - illiterate_rural_populations: excluded (powerless/trapped) — the majority outside the channel's door
 *   - censorship_authorities: payers of the policing cost (institutional/constrained) — enforcement repeatedly outrun by the medium
 *   - book_history_historians: analytical observer (analytical/analytical) — reads the structure across five centuries
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_composite__technological_mediation_reading, 0.22).
domain_priors:suppression_score(reformation_composite__technological_mediation_reading, 0.15).
domain_priors:theater_ratio(reformation_composite__technological_mediation_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_composite__technological_mediation_reading, mountain).
narrative_ontology:human_readable(reformation_composite__technological_mediation_reading, "Print Mediation of Religious Dissent, 1517-1555 (Technological Mediation Reading)").
narrative_ontology:topic_domain(reformation_composite__technological_mediation_reading, "historical epistemology/religious history/political economy").

domain_priors:emerges_naturally(reformation_composite__technological_mediation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_composite__technological_mediation_reading, 'c2f0ecca-785d-4b87-ad44-955bf791cd2e').
narrative_ontology:cs_kernel_codification('c2f0ecca-785d-4b87-ad44-955bf791cd2e', fixed_text).
narrative_ontology:cs_authority_grounding('c2f0ecca-785d-4b87-ad44-955bf791cd2e', expertise).
narrative_ontology:cs_interpretation_layer_present('c2f0ecca-785d-4b87-ad44-955bf791cd2e').
narrative_ontology:cs_reading_relation('c2f0ecca-785d-4b87-ad44-955bf791cd2e', reformation_composite__theological_fragmentation_reading, coexists_with).
narrative_ontology:cs_reading_relation('c2f0ecca-785d-4b87-ad44-955bf791cd2e', reformation_composite__political_realignment_reading, influences).
narrative_ontology:cs_axiom('c2f0ecca-785d-4b87-ad44-955bf791cd2e', foundational, mass_movement_requires_mass_medium).
narrative_ontology:cs_axiom_status(mass_movement_requires_mass_medium, holdable).
narrative_ontology:cs_axiom_grounding('c2f0ecca-785d-4b87-ad44-955bf791cd2e', mass_movement_requires_mass_medium, empirically_contingent).
narrative_ontology:cs_axiom('c2f0ecca-785d-4b87-ad44-955bf791cd2e', secondary, medium_properties_shape_movement_form).
narrative_ontology:cs_axiom_status(medium_properties_shape_movement_form, holdable).
narrative_ontology:cs_axiom_grounding('c2f0ecca-785d-4b87-ad44-955bf791cd2e', medium_properties_shape_movement_form, empirically_contingent).
narrative_ontology:cs_reference_frame('c2f0ecca-785d-4b87-ad44-955bf791cd2e', press_as_enabling_infrastructure).
narrative_ontology:cs_drift_state('c2f0ecca-785d-4b87-ad44-955bf791cd2e', contemporary_book_history_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('c2f0ecca-785d-4b87-ad44-955bf791cd2e', '2026-08-05T00:00:00Z').
narrative_ontology:cs_kernel_id(reformation_composite__technological_mediation_reading, reformation_composite).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_composite__technological_mediation_reading, printers_publishers).
narrative_ontology:constraint_beneficiary(reformation_composite__technological_mediation_reading, vernacular_reformist_authors).
narrative_ontology:constraint_beneficiary(reformation_composite__technological_mediation_reading, literate_urban_publics).
narrative_ontology:constraint_victim(reformation_composite__technological_mediation_reading, manuscript_culture_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reformation_composite__technological_mediation_reading, papal_imperial_church).
narrative_ontology:constraint_victim(reformation_composite__technological_mediation_reading, papal_imperial_church).
narrative_ontology:constraint_victim(reformation_composite__technological_mediation_reading, censorship_authorities).
narrative_ontology:constraint_vindicates(reformation_composite__technological_mediation_reading, media_determinism_thesis).
narrative_ontology:constraint_vindicates(reformation_composite__technological_mediation_reading, communication_infrastructure_enables_movements).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the workshops where every pamphlet, Bible translation, and polemic takes physical form. Revenue scales with output, and the boom in religious controversy after 1517 fills their order books from every side of the dispute. Within market demand and the licensing rules of whichever territory they sit in, they decide which manuscripts get set in type — a practical gate no party to the controversy can bypass, since all of them need the same workshops. Exit is realistic: a printer can switch to safe work such as Bibles, liturgies, and official proclamations when controversy turns dangerous, and press equipment moves between cities.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, printers_publishers, beneficiary,
    organized, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(reformation_composite__technological_mediation_reading, printers_publishers, agenda_setter).

% Write the short vernacular pamphlets that carry the controversy — sermons, satires, dialogues, woodcut-accompanied attacks. The channel turns a Wittenberg disputation into a text read across the continent within weeks, something no manuscript network could do. The same channel makes their commitments permanent and public: named authorship on a printed pamphlet cannot be walked back, and several of the most prolific pay for it with bans, excommunication, and exile. Leaving the channel means silence; staying in it means exposure.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, vernacular_reformist_authors, beneficiary,
    moderate, biographical, identity_locked, continental).

% Townsmen and townswomen with reading ability, or access to someone who reads aloud, who buy or hear the pamphlets and form the publics the controversy addresses. They gain direct access to arguments previously filtered through clerical mediation, and pay for it in book prices that remain far above daily wages even for the cheapest quartos. They can choose what to read and which city to live in, and urban reading culture grows around the flow.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, literate_urban_publics, beneficiary,
    moderate, biographical, mobile, regional).

% The hierarchy that held informational control over doctrine before 1517 through licensed preaching, manuscript standardization, and clerical mediation. An unlicensed pamphlet can now be reprinted faster than a ban can suppress it, and that control model does not recover. At the same time the Church is one of the channel's largest customers — indulgence campaigns, liturgical printing, and the Counter-Reformation's own output all run through the same presses. It cannot stop using print without abandoning its own standardization needs, and it cannot control print without enforcement machinery that repeatedly fails.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, papal_imperial_church, payer,
    institutional, generational, trapped, continental).
narrative_ontology:stakeholder_secondary_role(reformation_composite__technological_mediation_reading, papal_imperial_church, beneficiary).

% Professional scribes, scriptoria, and manuscript traders whose livelihood rested on hand-copying. Each press year displaces more of their market; their skill is the thing being displaced, and moving into the print trades means accepting piecework at the bottom of the new hierarchy. Their craft identity is bound to the manuscript tradition, which becomes uneconomic within a generation.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, manuscript_culture_practitioners, payer,
    powerless, biographical, identity_locked, regional).

% The majority of the population, bound to the land and without reading ability, for whom the channel is a closed door. They encounter the controversy only when a preacher or reader performs a pamphlet aloud, and their own grievances enter print only when literate intermediaries write them down — as with the 1525 peasants' articles, drafted for them and answered over their heads. They have no way to address the channel directly and no exit from the confessional conflicts it accelerates.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, illiterate_rural_populations, excluded,
    powerless, generational, trapped, regional).

% Imperial, princely, and papal offices charged with policing what the presses produce. Decentralized workshops, fast reprinting, and cross-border trade defeat their control model: every ban is outrun by the next edition. They pay for licensing bureaucracies, press visitations, and index machinery across decades, and the record of those expenditures — the repeated reissuance of failed mandates — is the clearest evidence of what the channel costs whoever tries to govern it.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, censorship_authorities, payer,
    institutional, generational, constrained, continental).

% Modern scholars of print culture who read the whole structure across five centuries: production statistics, literacy estimates, printer networks, and the revisionist debate over how much weight the medium bears. They collect nothing from the channel and pay nothing; their seat is analytical.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, book_history_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_composite__technological_mediation_reading, printers_publishers).
narrative_ontology:fixing_cost_class(reformation_composite__technological_mediation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the reproduction problem: once a text is set in type, identical copies can be produced in thousands at marginal cost near paper and press-time, replacing the manuscript regime's slow, error-accumulating, non-standardized copying. Every party to the controversy — reformers, the Church, princes, cities — uses the same solution to put identical doctrine in front of dispersed readers.
% TRANSFER_FUNCTION: Moves standardized text from authors through print workshops to dispersed literate publics across the continent; moves money from purchasers and patrons to printers and papermakers; moves attention toward the short vernacular pamphlet format the channel selects for; and moves informational gatekeeping out of clerical hands into the hands of whoever can finance and operate a press.
% ABSENT_VOICES: The illiterate rural majority — most of the population — would object that the controversy is being conducted over their heads in a medium they cannot address; they appear in print only when literate intermediaries transcribe their grievances and are answered by texts they cannot read. Manuscript-culture practitioners would object to having their displacement narrated as mere progress. Women, largely outside both Latin learning and the print trades, are nearly voiceless in the channel's output relative to their share of the population. All three sit outside the conversation the channel constitutes.
% DISAPPEARANCE_RATIONALE: Remove the channel's properties overnight — reproduction back to manuscript speed and cost — and the movement does not rearrange, it does not form: the 1517-1520 output stays a local academic dispute, Wittenberg does not become a publishing center, and the confessional map of 1555 does not emerge. Every party's recorded strategy presupposes the channel; the world of 1517-1555 is arranged around it.
% FOUNDING_PROBLEM: The founding problem of the print arrangement predates the controversy by two generations: books were scarce, expensive, and corrupted by hand-copying, and any institution needing identical documents in many places — the Church above all — needed cheap, accurate, standardized reproduction. The press was built and financed to solve that problem; the religious controversy of 1517 arrived into a solved-reproduction environment and exploited it.
% FOUNDING_PROBLEM_CORROBORATION: The problem's status is attested from outside the print economy's beneficiaries: the Church's own massive adoption of print for indulgences, liturgy, and Counter-Reformation output, and its construction of licensing and index machinery, corroborate that mass reproduction was the operative problem and print the operative solution; censorship authorities' decades of expenditure policing presses rather than scriptoria corroborate which channel mattered; and economic historians of the book, including the reading's critics, corroborate the reproduction-economics founding from the analytical seat.
narrative_ontology:disappearance_verdict(reformation_composite__technological_mediation_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_composite__technological_mediation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_composite__technological_mediation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reformation_composite__technological_mediation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_composite__technological_mediation_reading, 0.22, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_composite__technological_mediation_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, ExtMetricName, E),
    domain_priors:suppression_score(reformation_composite__technological_mediation_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(reformation_composite__technological_mediation_reading),
    narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(reformation_composite__technological_mediation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low (0.22) because the channel is infrastructure: its costs to users are capacity costs — capital, literacy, format conformity — not transfers to a rent collector, though capital concentration in the 1530s-40s gives the trade a modest gatekeeping rent, which the measurement series tracks rising from 0.14 on a single shared ten-point grid. Suppression is authored as a raw structural property (0.15) — unscaled by power or scope in this authoring; only extractiveness is scaled by the engine — because the channel coerces nothing: its exclusions (literacy, capital) are passive boundaries, and the active coercion in the record belongs to the censorship overlay, a separate downstream constraint. Theater is very low (0.08) because reproduction is functionally real; the small rise across the interval tracks licensed-imprint compliance performance growing with the censorship overlay, not atrophy of the channel's function. Accessibility collapse is high (0.78) for the channel's specific function — once print's economics are understood, no manuscript or oral alternative serves mass standardized dissemination, though oral networks persist for other functions. Resistance is low (0.10) because every party, including the channel's chief victim, adopted the medium; resistance in the record targets content through the channel, not the channel's properties.
 *
 * PERSPECTIVAL GAP:
 *   The same structure computes differently from each seat. From the printer's position the channel is a market and a livelihood that every party needs. From the reformer author's position it is liberation with a permanence trap — the medium that made their argument continental also made recantation useless. From the Church's position it is the collapse of centuries of informational control, run on machinery the Church itself must pay to use. From the scribe's position it is the displacement of the only trade they know. From the excluded rural majority's position it is a wall with voices on the other side of it. The engine computes this divergence from the declared power, exit, and role data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   printers_publishers are declared beneficiaries and collect the channel's revenue directly, so their directionality sits near the beneficiary end; their gatekeeping position is carried by the secondary agenda_setter role rather than by inflating their extraction. vernacular_reformist_authors and literate_urban_publics are beneficiaries whose payments are market prices for a product they demonstrably valued — damped extraction, not subsidy. manuscript_culture_practitioners are the declared victim group: the channel's operation transfers their market to the presses, so their directionality sits near the full-target end. papal_imperial_church is authored payer with a secondary beneficiary position: its net structural relationship is target (loss of informational control), partially damped by its own heavy use of the channel. censorship_authorities are payers whose cost is the failed policing of an ungovernable medium. illiterate_rural_populations are excluded rather than paying: the channel takes nothing from them; it simply never admits them. No directionality overrides are declared — the beneficiary/victim/role declarations plus exit options produce the correct d for every seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — cheap, accurate, standardized reproduction of texts — is live throughout the interval and remains live in successor media, so no mandatrophy is declared: the channel's function has not outlived itself in-period, and nothing here is maintained theatrically. The classification discipline runs in the other direction for this story: a technological artifact is CLAIMED as natural law, and the declared parties (the printers' gatekeeping rent, the scribes' displacement) give the engine what it needs to test that claim — if computed extraction exceeds what a physical constraint can carry, the false-summit signature fires, which is the correct diagnostic for a constructed arrangement wearing a natural-law frame. The omega variables hold the irreducible remainder: whether the artifact's properties count as natural once the artifact's existence was chosen, and whether the medium's contribution separates from the message it carried.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_artifact,
    'Is the print-mediation constraint a genuine physical constraint whose properties would hold regardless of who operated presses, or a constructed arrangement (press ownership concentration, capital access, literacy distribution) that identifiable parties built and benefit from?',
    'Counterfactual and comparative analysis: press-access regimes across territories (free imperial cities versus tightly licensed principalities) and the counterfactual diffusion under universalized access; if the channel''s operative properties vary with ownership and licensing regimes, the constructed component dominates.',
    'If constructed, the mountain claim fails and the constraint reclassifies toward a coordination/extraction hybrid with printers_publishers as the concentrated beneficiary; if the physical properties dominate, the mountain claim holds with the print economy as incidental beneficiary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_artifact, conceptual, 'Whether the press''s constraint status is natural (artifact properties) or constructed (ownership and literacy regimes).').

omega_variable(
    medium_message_separability,
    'Can the channel''s causal contribution to the mass movement be separated from the theological content it carried, or are medium and message one inseparable structure?',
    'Comparative print-history analysis: print-served movements with different content (astrology, failed heterodoxies, Catholic reform print) and the differential scaling of doctrinal positions through identical channels.',
    'If inseparable, this reading''s epsilon and classification are confounded with the theological_fragmentation_reading''s, and the two stories measure overlapping structure; if separable, the readings are cleanly independent constraints.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(medium_message_separability, conceptual, 'Whether technological mediation and theological content are structurally separable causes.').

omega_variable(
    literacy_exclusion_weight,
    'Does the channel''s literacy boundary constitute a cost borne by the illiterate majority, or is it a capacity property of the medium largely offset by oral re-mediation?',
    'Historical measurement of oral re-mediation reach (pamphlets read from pulpits, public readings, woodcut comprehension) and of illiterate actors'' own media (song, image, ritual) against the channel''s direct-access benefits.',
    'If the exclusion was severe and consequential, the channel''s effective extraction rises and the low-epsilon mountain profile weakens; if re-mediation substantially closed the access gap, epsilon stays low and the mountain profile holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literacy_exclusion_weight, empirical, 'Whether the literacy boundary is extraction-relevant cost or neutral capacity property.').

omega_variable(
    revisionist_challenge_status,
    'Has the revisionist critique of print determinism (the weight of preaching, image, and song; print''s equal service to the old church) overturned the reading''s foundational claim that the medium transformed local dissent into a mass movement?',
    'Continued archival quantification of non-print mediation channels'' reach and of print''s differential effect across the controversy''s actors.',
    'If overturned, this reading collapses from fundamental-cause to contributing-factor status, its mountain classification fails, and the kernel''s center of gravity moves toward the sibling readings; if it survives, the reference frame holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revisionist_challenge_status, empirical, 'Whether the revisionist historiographical challenge has overridden the reading''s foundational axiom.').

omega_variable(
    fundamental_characterization_disagreement,
    'Where exactly do the three readings of the reformation_composite kernel disagree structurally — on the cause of the movement''s scale (this reading: the medium), the content that scaled (theological reading: incompatible commitments), or the beneficiaries of the new arrangement (political reading: territorial sovereigns) — and can any single framework hold all three as fundamental?',
    'Framework analysis: test whether ''fundamental'' can be plural (medium as enabling condition, doctrine as content, sovereignty as beneficiary) without contradiction; if the readings are levels of one causal chain rather than rivals, the kernel decomposes into a single layered story rather than three competing ones.',
    'If the readings are levels of one chain, this story''s classification stands as the infrastructure layer and the sibling stories as content and beneficiary layers; if they are genuine rivals for the fundamental slot, only one can be fundamental and this reading''s enabling-constraint claim competes directly with the siblings'' claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fundamental_characterization_disagreement, conceptual, 'The location and resolvability of the inter-reading disagreement within the reformation_composite kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_composite__technological_mediation_reading, 1517, 1555).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1517, reformation_composite__technological_mediation_reading, theater_ratio, 1517, 0.04).
narrative_ontology:measurement_basis(refo_tr_t1517, observed).
narrative_ontology:measurement(refo_tr_t1522, reformation_composite__technological_mediation_reading, theater_ratio, 1522, 0.04).
narrative_ontology:measurement_basis(refo_tr_t1522, observed).
narrative_ontology:measurement(refo_tr_t1525, reformation_composite__technological_mediation_reading, theater_ratio, 1525, 0.05).
narrative_ontology:measurement_basis(refo_tr_t1525, observed).
narrative_ontology:measurement(refo_tr_t1530, reformation_composite__technological_mediation_reading, theater_ratio, 1530, 0.05).
narrative_ontology:measurement_basis(refo_tr_t1530, observed).
narrative_ontology:measurement(refo_tr_t1534, reformation_composite__technological_mediation_reading, theater_ratio, 1534, 0.05).
narrative_ontology:measurement_basis(refo_tr_t1534, observed).
narrative_ontology:measurement(refo_tr_t1538, reformation_composite__technological_mediation_reading, theater_ratio, 1538, 0.06).
narrative_ontology:measurement_basis(refo_tr_t1538, observed).
narrative_ontology:measurement(refo_tr_t1542, reformation_composite__technological_mediation_reading, theater_ratio, 1542, 0.06).
narrative_ontology:measurement_basis(refo_tr_t1542, observed).
narrative_ontology:measurement(refo_tr_t1546, reformation_composite__technological_mediation_reading, theater_ratio, 1546, 0.07).
narrative_ontology:measurement_basis(refo_tr_t1546, observed).
narrative_ontology:measurement(refo_tr_t1550, reformation_composite__technological_mediation_reading, theater_ratio, 1550, 0.07).
narrative_ontology:measurement_basis(refo_tr_t1550, observed).
narrative_ontology:measurement(refo_tr_t1555, reformation_composite__technological_mediation_reading, theater_ratio, 1555, 0.08).
narrative_ontology:measurement_basis(refo_tr_t1555, observed).

% Extraction over time
narrative_ontology:measurement(refo_be_t1517, reformation_composite__technological_mediation_reading, base_extractiveness, 1517, 0.14).
narrative_ontology:measurement_basis(refo_be_t1517, observed).
narrative_ontology:measurement(refo_be_t1522, reformation_composite__technological_mediation_reading, base_extractiveness, 1522, 0.15).
narrative_ontology:measurement_basis(refo_be_t1522, observed).
narrative_ontology:measurement(refo_be_t1525, reformation_composite__technological_mediation_reading, base_extractiveness, 1525, 0.16).
narrative_ontology:measurement_basis(refo_be_t1525, observed).
narrative_ontology:measurement(refo_be_t1530, reformation_composite__technological_mediation_reading, base_extractiveness, 1530, 0.17).
narrative_ontology:measurement_basis(refo_be_t1530, observed).
narrative_ontology:measurement(refo_be_t1534, reformation_composite__technological_mediation_reading, base_extractiveness, 1534, 0.18).
narrative_ontology:measurement_basis(refo_be_t1534, observed).
narrative_ontology:measurement(refo_be_t1538, reformation_composite__technological_mediation_reading, base_extractiveness, 1538, 0.19).
narrative_ontology:measurement_basis(refo_be_t1538, observed).
narrative_ontology:measurement(refo_be_t1542, reformation_composite__technological_mediation_reading, base_extractiveness, 1542, 0.2).
narrative_ontology:measurement_basis(refo_be_t1542, observed).
narrative_ontology:measurement(refo_be_t1546, reformation_composite__technological_mediation_reading, base_extractiveness, 1546, 0.21).
narrative_ontology:measurement_basis(refo_be_t1546, observed).
narrative_ontology:measurement(refo_be_t1550, reformation_composite__technological_mediation_reading, base_extractiveness, 1550, 0.22).
narrative_ontology:measurement_basis(refo_be_t1550, observed).
narrative_ontology:measurement(refo_be_t1555, reformation_composite__technological_mediation_reading, base_extractiveness, 1555, 0.22).
narrative_ontology:measurement_basis(refo_be_t1555, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(reformation_composite__technological_mediation_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_composite__technological_mediation_reading, global_infrastructure).
narrative_ontology:affects_constraint(reformation_composite__technological_mediation_reading, reformation_composite__theological_fragmentation_reading).
narrative_ontology:affects_constraint(reformation_composite__technological_mediation_reading, reformation_composite__political_realignment_reading).
narrative_ontology:affects_constraint(reformation_composite__technological_mediation_reading, imperial_press_censorship_regime).

% DUAL FORMULATION NOTE:
% The reformation_composite kernel decomposes into three epsilon-invariant readings — technological mediation (this story), theological fragmentation, and political realignment — because 'the Reformation's fundamental character' is a colloquial label covering structurally distinct claims with different observables, beneficiary structures, and epsilon values. This reading authors epsilon for the print-mediated communication arrangement, with publication rates and literacy as observables; the theological reading authors epsilon for the confessional-commitment structure; the political reading authors epsilon for the sovereignty-assertion arrangement. This reading is upstream of the censorship response (imperial_press_censorship_regime), exerts evidentiary influence on the political reading, and coexists with the theological reading as an orthogonal medium/message pair.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
