% ============================================================================
% CONSTRAINT STORY: herem_command_dt7__contextual_supersession_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_herem_command_dt7__contextual_supersession_reading, []).

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
 *   constraint_id: herem_command_dt7__contextual_supersession_reading
 *   human_readable: Herem Command (Deuteronomy 7) - Contextual Supersession Reading
 *   domain: biblical hermeneutics/religious ethics/commitment system analysis
 *
 * SUMMARY:
 *   Deuteronomy 7 commands the settling Israelites to make no covenant with,
 *   show no mercy to, intermarry with, or spare the nations of Canaan,
 *   destroying their cultic objects. This story instantiates ONE reading of
 *   that kernel - the contextual supersession reading - under which the
 *   command was a historically-bounded directive for the settlement period
 *   whose moral force was superseded by prophetic universalism and Christian
 *   covenantal inclusion. Under this reading the constraint's life arc runs
 *   from apex enforcement (total war, barred and dissolved marriages) through
 *   prophetic erosion to a completed retirement: today the command binds no
 *   one legitimately, intermarriage restrictions are relocated to consent and
 *   belief rather than ethnicity, and violence done in the command's name is
 *   delegitimated by the tradition's own moral development. The residual
 *   exception is a narrow set of insular communities that enforce separation
 *   citing the text - the only live victims. ASSUMPTIONS: the interval is
 *   indexed abstractly (t0 approximates the settlement-era enforcement apex;
 *   t60 approximates the present); base_properties scalars report the
 *   constraint's end-state operation, matching the final measurement points;
 *   the epsilon referent is the standing herem arrangement itself as this
 *   reading assesses it - severe in antiquity, near-nil now, with an
 *   illegitimate residue. The claim and the metrics are independent authored
 *   facts: the scaffold claim states what this reading takes the constraint
 *   structurally to be (a transitional instrument with an executed sunset);
 *   the metrics describe how it actually operates at interval end. Sibling
 *   readings are other files, not parts of this one.
 *
 * KEY AGENTS:
 *   - - ancient_israel_settlement_community: historical beneficiary (organized/trapped) - received land, security, and consolidated identity; paid in fighting, forgone marriages, and internal enforcement
 *   - - canaanite_outgroup_populations: historical primary target (powerless/trapped) - bore the command's full force; no seat in the tradition that recorded it
 *   - - mixed_marriage_families: historical target (moderate/constrained) - households dissolved at the boundary when enforcement tightened
 *   - - prophetic_and_apostolic_interpreters: agenda-setters of the retirement (institutional/mobile) - executed the supersession through reinterpretation
 *   - - scriptural_canon_custodians: current administrators (institutional/constrained) - maintain the text's place; collect standing from canon integrity, pay in perpetual explanation
 *   - - residual_fundamentalist_enforcers: anomalous post-sunset enforcers (organized/identity_locked) - capture the residual extraction; their office fuses with the frame they enforce
 *   - - residual_separation_enforcement_targets: current primary targets (powerless/identity_locked) - bear the last live coercion
 *   - - conscientious_believers_in_textual_traditions: beneficiaries of the retirement (moderate/constrained) - keep the canon without practicing the command
 *   - - contemporary_interfaith_ethicists: analytical observer (analytical/analytical) - sees the full structure from outside confessional authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(herem_command_dt7__contextual_supersession_reading, 0.14).
domain_priors:suppression_score(herem_command_dt7__contextual_supersession_reading, 0.22).
domain_priors:theater_ratio(herem_command_dt7__contextual_supersession_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, extractiveness, 0.14).
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(herem_command_dt7__contextual_supersession_reading, scaffold).
narrative_ontology:human_readable(herem_command_dt7__contextual_supersession_reading, "Herem Command (Deuteronomy 7) - Contextual Supersession Reading").
narrative_ontology:topic_domain(herem_command_dt7__contextual_supersession_reading, "biblical hermeneutics/religious ethics/commitment system analysis").

domain_priors:requires_active_enforcement(herem_command_dt7__contextual_supersession_reading).
narrative_ontology:has_sunset_clause(herem_command_dt7__contextual_supersession_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(herem_command_dt7__contextual_supersession_reading, '96f84a6e-bcc1-4217-93af-17ce91aad6ea').
narrative_ontology:cs_kernel_codification('96f84a6e-bcc1-4217-93af-17ce91aad6ea', fixed_text).
narrative_ontology:cs_authority_grounding('96f84a6e-bcc1-4217-93af-17ce91aad6ea', lineage).
narrative_ontology:cs_interpretation_layer_present('96f84a6e-bcc1-4217-93af-17ce91aad6ea').
narrative_ontology:cs_reading_relation('96f84a6e-bcc1-4217-93af-17ce91aad6ea', herem_command_dt7__durable_separation_reading, forecloses).
narrative_ontology:cs_reading_relation('96f84a6e-bcc1-4217-93af-17ce91aad6ea', herem_command_dt7__allegorical_displacement_reading, coexists_with).
narrative_ontology:cs_axiom('96f84a6e-bcc1-4217-93af-17ce91aad6ea', foundational, herem_scope_limited_to_settlement_era).
narrative_ontology:cs_axiom_status(herem_scope_limited_to_settlement_era, holdable).
narrative_ontology:cs_axiom_grounding('96f84a6e-bcc1-4217-93af-17ce91aad6ea', herem_scope_limited_to_settlement_era, empirically_contingent).
narrative_ontology:cs_axiom('96f84a6e-bcc1-4217-93af-17ce91aad6ea', foundational, prophetic_universalism_morally_supersedes_herem).
narrative_ontology:cs_axiom_status(prophetic_universalism_morally_supersedes_herem, holdable).
narrative_ontology:cs_axiom_grounding('96f84a6e-bcc1-4217-93af-17ce91aad6ea', prophetic_universalism_morally_supersedes_herem, deontological).
narrative_ontology:cs_reference_frame('96f84a6e-bcc1-4217-93af-17ce91aad6ea', settlement_scoped_command_morally_retired).
narrative_ontology:cs_drift_state('96f84a6e-bcc1-4217-93af-17ce91aad6ea', contemporary, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('96f84a6e-bcc1-4217-93af-17ce91aad6ea', '').
narrative_ontology:cs_kernel_id(herem_command_dt7__contextual_supersession_reading, herem_command_dt7).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(herem_command_dt7__contextual_supersession_reading, ancient_israel_settlement_community).
narrative_ontology:constraint_beneficiary(herem_command_dt7__contextual_supersession_reading, scriptural_canon_custodians).
narrative_ontology:constraint_beneficiary(herem_command_dt7__contextual_supersession_reading, conscientious_believers_in_textual_traditions).
narrative_ontology:constraint_victim(herem_command_dt7__contextual_supersession_reading, canaanite_outgroup_populations).
narrative_ontology:constraint_victim(herem_command_dt7__contextual_supersession_reading, mixed_marriage_families).
narrative_ontology:constraint_victim(herem_command_dt7__contextual_supersession_reading, residual_separation_enforcement_targets).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(herem_command_dt7__contextual_supersession_reading, residual_fundamentalist_enforcers).
narrative_ontology:constraint_victim(herem_command_dt7__contextual_supersession_reading, ancient_israel_settlement_community).
narrative_ontology:constraint_vindicates(herem_command_dt7__contextual_supersession_reading, prophetic_universalism_doctrine).
narrative_ontology:constraint_vindicates(herem_command_dt7__contextual_supersession_reading, christian_covenantal_inclusion_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Entered a region of fortified city-states as a tribal confederation without a monopoly of force. The command gave it a unified war policy and a membership boundary at the moment its survival as a distinct polity was least secure. It received the land, security, and consolidated identity the campaign produced, and it paid in kind: its fighters bore the fighting, its households bore the ban on marriages many of them wanted, and its courts administered penalties against members who kept forbidden plunder or spared forbidden peoples. Membership was not exitable; the boundary ran through its own families.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, ancient_israel_settlement_community, beneficiary,
    organized, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(herem_command_dt7__contextual_supersession_reading, ancient_israel_settlement_community, payer).

% Held the cities, farms, and shrines the settling confederation entered. The command denied them treaty, mercy, and incorporation: they faced warfare aimed at destruction, loss of land and sanctuary, and the breaking of their cultic and family life. Flight was the only exit, partial and precarious, and the command's terms left no route to becoming neighbors. They had no voice in the tradition that recorded, interpreted, and eventually retired the command; they appear in it only as its object.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, canaanite_outgroup_populations, payer,
    powerless, biographical, trapped, regional).

% Households formed across the boundary line the command drew. When enforcement tightened, these families were the point of application: marriages dissolved, spouses and children sent away, inheritance and belonging cancelled. Their options were dissolution or concealment; open continuance invited communal penalty. Later generations of readers inherit their stories as the human cost of the boundary policy.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, mixed_marriage_families, payer,
    moderate, biographical, constrained, regional).

% Poets, prophets, and later apostles working inside the same scriptural tradition. They attacked the sacrifice-and-exclusion reading of the community's vocation, asserted that the nations share the holy mountain, folded a Moabite ancestress into the royal line, and argued inclusion into the covenant without ethnic boundary-keeping. Their interpretive labor is what retired the command's force while leaving the text in place. They could move among texts and audiences because they controlled neither armies nor courts; their instrument was reinterpretation itself.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, prophetic_and_apostolic_interpreters, agenda_setter,
    institutional, civilizational, mobile, regional).

% Rabbinic academies, churches, and their successors who transmit the canon as a unit. They decide where the passages sit in lectionaries and curricula, fund the commentaries that explain their non-application, and answer for the text to converts, critics, and descendant communities. Removing the passages would fracture doctrines of canon and inspiration they depend on; keeping them obliges permanent interpretive maintenance. They collect standing from the canon's integrity and pay in perpetual explanation.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, scriptural_canon_custodians, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(herem_command_dt7__contextual_supersession_reading, scriptural_canon_custodians, beneficiary).

% Leaders of insular communities who teach the separation commands as presently binding and police courtship, marriage, and association accordingly - through shaming, shunning, control of matchmaking, and expulsion. Their authority rests on the claim that the old boundary still stands; each enforcement episode renews it. Abandoning that claim would dissolve the office that holds them, so the frame and the office are fused.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, residual_fundamentalist_enforcers, agenda_setter,
    organized, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(herem_command_dt7__contextual_supersession_reading, residual_fundamentalist_enforcers, beneficiary).

% Members of those insular communities - mostly the young and the unmarried - whose friendships, courtships, and marriages are vetted against the old boundary. Their schooling, work, and family ties sit inside the community; exit means losing all of them at once, and the piety they were raised in tells them the loss would be deserved. Coalition with other targets is difficult because each member's social world is contained within the enforcing group.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, residual_separation_enforcement_targets, payer,
    powerless, biographical, identity_locked, local).

% Ordinary members of synagogues and churches who meet these passages in lectionary and classroom. The supersession settlement is what lets them keep the canon without practicing or endorsing the command: they may marry across ethnic and religious lines, worship alongside outsiders, and read the passages as cautionary history. What they pay is attention - the recurring labor of hearing hard texts explained.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, conscientious_believers_in_textual_traditions, beneficiary,
    moderate, biographical, constrained, global).

% Academic scholars of religion, hermeneutics, and ethics who study the command and its readings from outside confessional authority. They trace the reception history, compare the readings' moral outputs, and publish assessments the tradition may ignore but cannot silence. Their seat carries no enforcement power and collects no revenue from the arrangement.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, contemporary_interfaith_ethicists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(herem_command_dt7__contextual_supersession_reading, residual_fundamentalist_enforcers).
narrative_ontology:fixing_cost_class(herem_command_dt7__contextual_supersession_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinated a fragile tribal confederation's settlement of contested territory: unified military action against entrenched city-states, prevention of defection through assimilation, and consolidation of a distinct covenant identity during a formative window when the community's survival as a separate polity was uncertain.
% TRANSFER_FUNCTION: Moved land, autonomy, and life itself from the region's incumbent populations to the settling community; moved marriageable persons and household continuity out of reach of cross-boundary families; and, after the command's force expired, moved interpretive labor onto every subsequent generation of readers, who must explain why the text no longer binds.
% ABSENT_VOICES: The commanded-upon populations - the Canaanite peoples and the cross-boundary families - had no seat in the tradition that recorded, interpreted, and retired the command; their testimony survives only as the object of the text. Contemporary voices from descendant communities and from inside insular groups subject to residual enforcement are likewise outside the interpretive councils that pronounce the command superseded. Their absence is what allows the retirement to feel costless to the institutions announcing it.
% DISAPPEARANCE_RATIONALE: If the command and its interpretive apparatus vanished overnight, the canon would lose chapters that custodians currently defend and explain, lectionaries and curricula would drop the passages, residual fundamentalist communities would lose their proof-text for enforced separation - dissolving the last live coercion - and the tradition's moral-development narrative, the arc from commanded exclusion to universal inclusion, would lose its starting point. Historically, the world literally rearranged around the command when it was in force; today a smaller but real rearrangement would follow its disappearance.
% FOUNDING_PROBLEM: A landless tribal confederation entering territory held by fortified city-states needed to act with unity and prevent its members' absorption into the incumbent populations; the command fused military policy with membership boundary to secure the settlement project.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration from outside the benefiting parties: non-confessional ancient-Levant scholarship (highland-settlement archaeology, Egyptian and Mesopotamian records) attests that the settlement-era boundary-and-survival problem was real and time-bound. Internal-but-adversarial witnesses - Ruth, Jonah, Isaiah 56, Amos 9 - attest that the tradition itself registered the exclusion program as repudiated and the founding problem as resolved. Descendant and affected communities attest the historical harm while attesting no continuing legitimacy for the command. No party outside the benefiting traditions attests that the founding problem remains live.
narrative_ontology:disappearance_verdict(herem_command_dt7__contextual_supersession_reading, world_rearranges).
narrative_ontology:founding_problem_status(herem_command_dt7__contextual_supersession_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(herem_command_dt7__contextual_supersession_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(herem_command_dt7__contextual_supersession_reading, 'none', 1).
narrative_ontology:epsilon_provenance(herem_command_dt7__contextual_supersession_reading, 0.14, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(herem_command_dt7__contextual_supersession_reading_tests).
:- end_tests(herem_command_dt7__contextual_supersession_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at interval end is 0.14: the command's legitimate force is nil under this reading, and the only live costs are borne by members of insular communities where the text is still enforced - a deliberately narrow victim set. Suppression is 0.22 and is authored as a RAW structural property (the engine scales only extractiveness, by directionality and scope): it consists of intra-communal social coercion - shaming, shunning, matchmaking control - not military or judicial machinery, which dissolved centuries ago. Theater_ratio is 0.42: a large share of current engagement with the text is performative maintenance (lectionary recitation, sermons explaining non-application, apologetics) but real hermeneutical and moral-instruction function remains, so the ratio sits below the atrophy threshold. Accessibility_collapse is 0.25: once the constraint is understood as superseded, alternatives stand wide open - intermarriage, interfaith worship, and conscience are all available; only inside the residual pockets do alternatives collapse. Resistance is 0.45: the retirement itself met historic resistance from the command's partisans, and the residual pockets meet outsized resistance relative to their size (exit movements, exposure journalism, external legal scrutiny in extreme cases), while the mainstream arrangement demands little and therefore receives little. The measurement series run on one shared grid (t = 0,10,20,30,40,50,60) so every metric is authored at every examined point; the trajectories show monotonic enforcement decay, not oscillation - the sunset fired gradually through prophetic and apostolic reinterpretation rather than in one act. On coalition power: the historical targets could not coordinate against the command militarily, and the current targets cannot coordinate socially because each member's entire social world is contained within the enforcing group - identity lock substitutes for external barriers.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the custodian seat the arrangement looks like heritage stewardship - a demanding but benign duty of explanation, nearer a rope in texture. From the residual target seat the same text operates as immediate coercion with no exit that does not cost family, livelihood, and faith-community at once - a snare-shaped pocket. From the ethicist seat the whole is a completed transition whose remaining interest is historiographic. The engine derives this divergence from the structural data (power, exit, role); the authored scaffold claim does not adjudicate between the seats. Identity-lock dynamics bind the two residual seats in opposite directions: for the enforcers the lock is institutional (the office has become the frame - abandoning enforcement dissolves the authority that holds them); for the targets it is relational and ideological (self-concept constituted through the community, plus a piety that pre-interprets exit as betrayal). If the identity frame broke in either seat - enforcers conceding the command's scope was historical, targets discovering life outside - the residual pocket's coercion profile would collapse toward the mainstream zero.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation, and no directionality overrides are needed: the declarations plus exit atoms already differentiate the seats. The settlement community derives a low d (declared beneficiary) moderated upward slightly by its secondary payer position and trapped exit. The Canaanite populations derive near-full-target d: declared victims, powerless, trapped - the constraint subsidized the settlers at their total expense. Mixed-marriage families derive high d as declared victims with constrained exit. Residual enforcement targets derive the highest d in the story: victims whose identity_locked exit places them nearest the full-target end, since trapped-or-locked targets amplify effective extraction. Residual enforcers derive low d - they sit on the collecting side of the only live extraction, which is why gain_flow names that seat. Custodians derive mildly beneficiary d through their secondary beneficiary role (standing collected from canon integrity). Conscientious believers derive low d (pure beneficiaries of the retirement). The prophetic and apostolic interpreters carry no beneficiary or victim declaration because their relationship to the constraint was neither - they dismantled it - and their seat is treated as near-symmetric administrative labor. Spatial scopes are assigned honestly: the historical seats are regional, the residual pocket is local, the custodial and believer seats are global, which the engine weighs in scaling effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The R5 interview resolves decisively under this reading: the founding problem (settlement survival against absorption) died millennia ago, and the arrangement persists as canon text plus interpretive labor. The mismatch consumer will read founding_problem_status=dead against disappearance_verdict=world_rearranges and flag a zombie/capture signature - correctly, since the text's persistence is maintained by custodial stewardship and residual enforcement rather than by its founding function, and the theater_ratio trajectory (0.08 to 0.42) documents exactly that substitution of performance for function. Declaring the mandatrophy resolved is the whole point of this reading: the supersession claim IS the sunset firing. The classification prevents two opposite mislabels. Calling the whole constraint a snare erases the genuine coordination the command once performed for a fragile confederation and flattens the historical beneficiaries into mere predators. Calling it a rope erases the Canaanite victims, the dissolved families, and the residual coerced targets. Scaffold names the structural truth this reading asserts: a real transitional instrument, justified by the transition rather than a steady state, whose sunset clause has been executed by the tradition's own prophetic and apostolic hands - with a snare-shaped residue confined to insular pockets and a piton-flavored question (costly-to-remove text, theatrical maintenance) attached to the canon shell the instrument left behind.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This story instantiates the contextual_supersession_reading of the herem_command_dt7 kernel (Deuteronomy 7''s ban on covenant, mercy, and intermarriage with the Canaanite nations). Which reading governs the command''s present normative force - historically-bounded-and-morally-superseded (this reading), timeless divine mandate for categorical separation (durable_separation_reading), or purely typological combat against sin (allegorical_displacement_reading)? The disagreement is located in two specific structural elements: the command''s temporal scope (expired vs. perpetual) and its present-day addressees (no one vs. all covenant members vs. no ethnic referent at all).',
    'Comparative hermeneutical adjudication: shifts in exegetical consensus, confessional statements, and liturgical practice would reveal which reading the living tradition actually operates under; adoption rates of each reading across denominations are directly observable.',
    'Adopting the durable_separation_reading would reinstate an ethnic separation duty, expand the victim set to all cross-boundary couples in believing communities, and drive extractiveness sharply upward; adopting the allegorical_displacement_reading would dissolve the ethnic referent entirely, emptying the victim set and converting the constraint into an internal-spiritual discipline with near-zero interpersonal cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which of three readings of the Deuteronomy 7 kernel controls the command''s present force, and where the readings structurally diverge.').

omega_variable(
    residual_enforcement_trajectory,
    'Will residual fundamentalist enforcement of separation norms citing Deuteronomy 7 decay through assimilation, literacy, and exit, or grow through sectarian revival?',
    'Longitudinal study of insular communities: enforcement incident rates, exit-interview data, demographic retention curves, and the incidence of the command cited in marriage-control cases over time.',
    'Growth would raise suppression and extractiveness above the authored end-state values and push the computed residue toward a snare-shaped pocket; decay completes the scaffold''s retirement and drives theater_ratio further up as the only remaining activity becomes explanatory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_enforcement_trajectory, empirical, 'Whether the last live enforcement pocket is expanding or disappearing.').

omega_variable(
    residual_suppression_mechanism,
    'Is the suppression measured in residual enforcement communities structural (closed social economics, lost schooling and work networks) or internalized (piety-fused consent in which members experience the boundary as devotion)?',
    'Post-exit suppression trajectory: if leavers continue to police their own relationships against the old boundary after physical exit, the suppression is substantially internalized; if it lapses on exit, it was structural.',
    'If internalized, effective suppression exceeds the structural measure - the constraint travels with the leaver - and the narrow victim set understates the true population bearing costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_suppression_mechanism, empirical, 'Structural vs. internalized character of the residual enforcement''s coercive force.').

omega_variable(
    supersession_legitimacy_extraction,
    'Does the supersession reading itself carry a legitimacy rent - harvesting moral authority for the tradition from its self-narrated transcendence of the command while the canon retains the command''s prestige and the custodians collect standing for stewarding the reconciliation?',
    'Compare moral-authority flows across traditions that retained-and-reinterpreted the passages versus traditions that formally repented or excised them: if reinterpretation generates deference and institutional standing disproportionate to the harm reduction achieved, a rent stream exists.',
    'If yes, part of the low authored extractiveness is offset by a legitimacy stream accruing to the custodian seat, tilting the computed residue toward a hybrid coordination-plus-collection shape rather than clean retirement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(supersession_legitimacy_extraction, conceptual, 'Whether the retirement narrative itself generates an authority rent for its administrators.').

omega_variable(
    intermarriage_pressure_boundary,
    'Does soft intermarriage pressure in mainstream believing communities constitute continued operation of this constraint, or a distinct endogamy preference no longer authorized by the text?',
    'Attribution analysis: survey whether mainstream communities cite Deuteronomy 7 (or its kernel) in opposing intermarriage, or rest on generic in-group preference; code sermons, counseling materials, and parental justifications.',
    'If the text is causally cited, the victim set widens well beyond residual fundamentalist pockets and the authored extractiveness of 0.14 is understated; if uncited, the constraint''s intermarriage operation is genuinely retired and relocated to consent and belief as this reading claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intermarriage_pressure_boundary, empirical, 'Where the boundary of the contemporary victim set sits - insular pockets only, or mainstream intermarriage pressure too.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(herem_command_dt7__contextual_supersession_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(here_tr_t0, herem_command_dt7__contextual_supersession_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(here_tr_t0, observed).
narrative_ontology:measurement(here_tr_t10, herem_command_dt7__contextual_supersession_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement_basis(here_tr_t10, observed).
narrative_ontology:measurement(here_tr_t20, herem_command_dt7__contextual_supersession_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement_basis(here_tr_t20, observed).
narrative_ontology:measurement(here_tr_t30, herem_command_dt7__contextual_supersession_reading, theater_ratio, 30, 0.34).
narrative_ontology:measurement_basis(here_tr_t30, observed).
narrative_ontology:measurement(here_tr_t40, herem_command_dt7__contextual_supersession_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement_basis(here_tr_t40, observed).
narrative_ontology:measurement(here_tr_t50, herem_command_dt7__contextual_supersession_reading, theater_ratio, 50, 0.44).
narrative_ontology:measurement_basis(here_tr_t50, observed).
narrative_ontology:measurement(here_tr_t60, herem_command_dt7__contextual_supersession_reading, theater_ratio, 60, 0.42).
narrative_ontology:measurement_basis(here_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(here_be_t0, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 0, 0.88).
narrative_ontology:measurement_basis(here_be_t0, observed).
narrative_ontology:measurement(here_be_t10, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 10, 0.8).
narrative_ontology:measurement_basis(here_be_t10, observed).
narrative_ontology:measurement(here_be_t20, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement_basis(here_be_t20, observed).
narrative_ontology:measurement(here_be_t30, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 30, 0.45).
narrative_ontology:measurement_basis(here_be_t30, observed).
narrative_ontology:measurement(here_be_t40, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 40, 0.28).
narrative_ontology:measurement_basis(here_be_t40, observed).
narrative_ontology:measurement(here_be_t50, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 50, 0.16).
narrative_ontology:measurement_basis(here_be_t50, observed).
narrative_ontology:measurement(here_be_t60, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 60, 0.14).
narrative_ontology:measurement_basis(here_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(here_su_t0, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement_basis(here_su_t0, observed).
narrative_ontology:measurement(here_su_t10, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 10, 0.78).
narrative_ontology:measurement_basis(here_su_t10, observed).
narrative_ontology:measurement(here_su_t20, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement_basis(here_su_t20, observed).
narrative_ontology:measurement(here_su_t30, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement_basis(here_su_t30, observed).
narrative_ontology:measurement(here_su_t40, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 40, 0.38).
narrative_ontology:measurement_basis(here_su_t40, observed).
narrative_ontology:measurement(here_su_t50, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 50, 0.26).
narrative_ontology:measurement_basis(here_su_t50, observed).
narrative_ontology:measurement(here_su_t60, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 60, 0.22).
narrative_ontology:measurement_basis(here_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(herem_command_dt7__contextual_supersession_reading, identity_coordination).
narrative_ontology:affects_constraint(herem_command_dt7__contextual_supersession_reading, herem_command_dt7__durable_separation_reading).
narrative_ontology:affects_constraint(herem_command_dt7__contextual_supersession_reading, herem_command_dt7__allegorical_displacement_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the herem command' decomposes into three structurally distinct constraints corresponding to three readings of one kernel (herem_command_dt7). This story authors the contextual supersession reading: a bounded settlement-era instrument, morally retired, with low present extractiveness and a narrow contemporary victim set. The durable_separation_reading authors the same text as a timeless mandate - high present extractiveness, victim set expanded to all cross-boundary couples in believing communities. The allegorical_displacement_reading dissolves the ethnic referent entirely - victims relocate to those disciplined by spiritualized-combat frameworks. Each story carries its own epsilon, beneficiaries, and victims per the epsilon-invariance principle; they are linked here as a constraint family. Structural pressure runs from this reading toward the durable reading (every successful supersession argument drains the durable reading's legitimacy conditions) while the allegorical reading parasitically stabilizes both by absorbing the text's violence into metaphor.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
