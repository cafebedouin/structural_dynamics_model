% ============================================================================
% CONSTRAINT STORY: orthographic_legitimacy_kernel__modernist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_legitimacy_kernel__modernist_reading, []).

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
 *   constraint_id: orthographic_legitimacy_kernel__modernist_reading
 *   human_readable: Modernist Reading of Orthographic Legitimacy: Script Rupture as Civilizational Alignment
 *   domain: political/linguistic/state_formation
 *
 * SUMMARY:
 *   The modernist reading of the orthographic-legitimacy kernel holds that a
 *   nation's writing system is legitimate insofar as it aligns that nation
 *   with Western/European modernity and severs it from the Ottoman/Islamic
 *   textual past. Institutionalized in the 1928 Turkish Alphabet Law and its
 *   enforcement regime, this doctrine made the new Latin orthography the sole
 *   gateway to public life: schooling, bureaucracy, law, and print ran
 *   exclusively through it, while the Arabic-letter competence of the old
 *   literate classes ceased to count. The reform delivered real coordination
 *   goods — a phonetic, machine-compatible, mass-teachable national script —
 *   while simultaneously dispossessing the ulema, the divan-trained clerks,
 *   the calligraphy guilds, and the endowed institutions whose authority and
 *   livelihood were constituted in the old letters. Script change here is
 *   constitutive of identity transformation, not merely instrumental:
 *   rendering the old carriers of textual authority illegible was part of the
 *   point, because their authority was an obstacle to the civilizational
 *   rupture the doctrine required. KEY AGENTS (by structural relationship): -
 *   kemalist_state_apparatus: Agenda-setter and principal collector
 *   (institutional/arbitrage) — writes and enforces the rule, collects the
 *   administrative and ideological dividend - ulema_religious_scholars:
 *   Primary target (moderate/identity_locked) — bears dispossession of
 *   vocation, library, and public voice - ottoman_bureaucratic_scribes:
 *   Primary target (moderate/trapped) — professional capital invalidated
 *   mid-career - calligraphy_and_manuscript_craftsmen: Secondary target
 *   (powerless/trapped) — craft economy extinguished -
 *   islamic_endowment_institutions: Institutional target (organized/trapped)
 *   — holdings intact, circulation severed -
 *   rural_literacy_target_population: Dual-positioned (powerless/constrained)
 *   — intended beneficiary, bearing generational transition costs -
 *   secular_westernized_intelligentsia: Secondary beneficiary
 *   (powerful/mobile) — careers built on the new credential -
 *   continuity_partisans: Excluded seat (moderate/constrained) — would
 *   object, was removed from the room - foreign_modernity_interlocutors and
 *   sociolinguistic_analysts: Analytical observers (institutional/analytical,
 *   analytical/analytical) FAMILY NOTE (epsilon-invariance decomposition):
 *   this file is one member of a three-story constraint family decomposing
 *   the colloquial label 'alphabet reform'. The modernist reading authors
 *   epsilon ~= 0.74 over the rupture-arrangement as this reading's own lights
 *   assess it: rupture is legitimate transformation, but the dispossession of
 *   the old literate classes is a real cost borne to purchase it, and the
 *   reading does not deny the cost. The continuity sibling authors much
 *   higher effective loss over the same arrangement (maximal severance of
 *   transmission; the entire tradition-bearing class counts as harmed), and
 *   the instrumentalist sibling authors substantially lower extraction (the
 *   arrangement evaluated purely as literacy-and-efficiency delivery shifts
 *   rural populations toward the beneficiary pole). Same Latin fact pattern,
 *   three different constraints, three different epsilon values — linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - - kemalist_state_apparatus: Agenda-setter and principal collector (institutional/arbitrage)
 *   - - ulema_religious_scholars: Primary target (moderate/identity_locked)
 *   - - ottoman_bureaucratic_scribes: Primary target (moderate/trapped)
 *   - - calligraphy_and_manuscript_craftsmen: Secondary target (powerless/trapped)
 *   - - islamic_endowment_institutions: Institutional target (organized/trapped)
 *   - - rural_literacy_target_population: Dual-positioned beneficiary/payer (powerless/constrained)
 *   - - secular_westernized_intelligentsia: Secondary beneficiary (powerful/mobile)
 *   - - continuity_partisans: Excluded seat (moderate/constrained)
 *   - - foreign_modernity_interlocutors: Analytical observer (institutional/analytical)
 *   - - sociolinguistic_analysts: Analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_legitimacy_kernel__modernist_reading, 0.74).
domain_priors:suppression_score(orthographic_legitimacy_kernel__modernist_reading, 0.42).
domain_priors:theater_ratio(orthographic_legitimacy_kernel__modernist_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__modernist_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__modernist_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__modernist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_legitimacy_kernel__modernist_reading, tangled_rope).
narrative_ontology:human_readable(orthographic_legitimacy_kernel__modernist_reading, "Modernist Reading of Orthographic Legitimacy: Script Rupture as Civilizational Alignment").
narrative_ontology:topic_domain(orthographic_legitimacy_kernel__modernist_reading, "political/linguistic/state_formation").

domain_priors:requires_active_enforcement(orthographic_legitimacy_kernel__modernist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_legitimacy_kernel__modernist_reading, '96843e39-eead-4801-9c0c-1b9d6da9db6b').
narrative_ontology:cs_kernel_codification('96843e39-eead-4801-9c0c-1b9d6da9db6b', formalized).
narrative_ontology:cs_authority_grounding('96843e39-eead-4801-9c0c-1b9d6da9db6b', extraction).
narrative_ontology:cs_interpretation_layer_present('96843e39-eead-4801-9c0c-1b9d6da9db6b').
narrative_ontology:cs_reading_relation('96843e39-eead-4801-9c0c-1b9d6da9db6b', orthographic_legitimacy_kernel__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('96843e39-eead-4801-9c0c-1b9d6da9db6b', orthographic_legitimacy_kernel__instrumentalist_reading, influences).
narrative_ontology:cs_axiom('96843e39-eead-4801-9c0c-1b9d6da9db6b', foundational, civilizational_alignment_constitutes_legitimacy).
narrative_ontology:cs_axiom_status(civilizational_alignment_constitutes_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('96843e39-eead-4801-9c0c-1b9d6da9db6b', civilizational_alignment_constitutes_legitimacy, empirically_contingent).
narrative_ontology:cs_axiom('96843e39-eead-4801-9c0c-1b9d6da9db6b', foundational, rupture_from_confessional_past_emancipates_nation).
narrative_ontology:cs_axiom_status(rupture_from_confessional_past_emancipates_nation, holdable).
narrative_ontology:cs_axiom_grounding('96843e39-eead-4801-9c0c-1b9d6da9db6b', rupture_from_confessional_past_emancipates_nation, deontological).
narrative_ontology:cs_reference_frame('96843e39-eead-4801-9c0c-1b9d6da9db6b', western_aligned_national_modernity).
narrative_ontology:cs_drift_state('96843e39-eead-4801-9c0c-1b9d6da9db6b', contemporary_multiple_modernities_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('96843e39-eead-4801-9c0c-1b9d6da9db6b', '').
narrative_ontology:cs_kernel_id(orthographic_legitimacy_kernel__modernist_reading, orthographic_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__modernist_reading, kemalist_state_apparatus).
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__modernist_reading, secular_westernized_intelligentsia).
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__modernist_reading, rural_literacy_target_population).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__modernist_reading, ulema_religious_scholars).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__modernist_reading, ottoman_bureaucratic_scribes).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__modernist_reading, calligraphy_and_manuscript_craftsmen).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__modernist_reading, islamic_endowment_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__modernist_reading, rural_literacy_target_population).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Directs the 1928 Alphabet Law, the Millet Mektepleri literacy campaigns, school curricula, press licensing, and the language societies that police orthographic practice. Gains a uniform script usable across ministries, courts, and the army, plus a cadre of officials trained wholly inside the new system. Its members write the rules they live by; leaving the arrangement would mean dismantling their own command posts.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, kemalist_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Teachers, lawyers, journalists, and engineers formed in republican or European-run schools whose Latin-letter credentials become the sole currency of public office. Careers, publication venues, and social standing flow through the new script. Mobile between professions, provinces, and increasingly between countries.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, secular_westernized_intelligentsia, beneficiary,
    powerful, biographical, mobile, national).

% Peasant and small-town families enrolled in night literacy courses after 1928. Children learn to read contracts, petitions, and ballots in the new letters; the same households lose the ability to read gravestones, family letters, property deeds, and prayer books inherited from grandparents. Moving elsewhere is not a realistic option; the cost lands as a split between generations inside one household.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, rural_literacy_target_population, beneficiary,
    powerless, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(orthographic_legitimacy_kernel__modernist_reading, rural_literacy_target_population, payer).

% Jurists, preachers, and teachers of the medrese tradition whose entire working library — Qur'anic commentary, jurisprudence, poetry, calligraphy manuals — is written in Arabic letters. After 1928 their credentials certify competence the state no longer recognizes, their students are recruited into Latin-script schools, and their public voice narrows to mosque precincts. Leaving means abandoning the vocation that constitutes them; staying means shrinking into cultural obsolescence. Many continue teaching scripture privately.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, ulema_religious_scholars, payer,
    moderate, biographical, identity_locked, continental).

% Career clerks trained in the divan hand, often two decades of service, whose penmanship and command of chancery formulas constitute the offices' institutional memory. Ministries convert their ledgers and letterheads within months of the law; men in their forties retrain as typists or take early retirement. A minority land well in the private sector; the rest watch juniors holding Latin typing certificates pass them.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, ottoman_bureaucratic_scribes, payer,
    moderate, biographical, trapped, national).

% Guild-trained masters of the classical scripts whose commissions came from mosques, officials, and wealthy patrons ordering illuminated panels and inscribed works. Official demand disappears almost overnight; the remaining market is tourist souvenirs and private devotion. The skill took decades to acquire and has no transfer path into the new typography.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, calligraphy_and_manuscript_craftsmen, payer,
    powerless, immediate, trapped, regional).

% Waqf administrations and library boards holding centuries of endowed manuscripts, school buildings, and charitable income earmarked for traditional education. The holdings remain physically intact but stop circulating: catalogued in Arabic letters, unreadable to new school graduates, audited by officials requesting Latin summaries. Consulting their own collections requires hiring the dwindling specialists who can still read them.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, islamic_endowment_institutions, payer,
    organized, generational, trapped, continental).

% Educators, historians, and clergy who argued through the 1920s for a simplified Arabic-based orthography or a phased bilingual transition, and later for parallel teaching of the Ottoman script in secondary schools. Kept off the Alphabet Commission; their proposals are tabled as sentimental or reactionary; several lose positions. They publish memoirs, keep quiet seminar rooms, and resurface periodically in parliamentary debate and the press.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, continuity_partisans, excluded,
    moderate, biographical, constrained, national).

% European advisors, orientalist correspondents, and later comparative modernizers who supply the yardstick the reform measures itself against. Their approval — press coverage, advisory missions, conference invitations — flows to the state, and their assessments of progress are quoted in textbooks. They observe from outside the polity and bear none of its adjustment costs.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, foreign_modernity_interlocutors, observer,
    institutional, generational, analytical, continental).

% Historical linguists and sociologists tracing the reform's outcomes across generations: literacy curves, archive-access bottlenecks, and comparisons with the Latinization episodes in Central Asia and the Caucasus. They see the whole distribution of gains and losses and hold no stake in any faction's vindication.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, sociolinguistic_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(orthographic_legitimacy_kernel__modernist_reading, kemalist_state_apparatus).
narrative_ontology:fixing_cost_class(orthographic_legitimacy_kernel__modernist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A newly consolidated nation-state needed one standardized national orthography compatible with contemporary printing, telegraphy, and typewriting, and teachable to a mass school population within a single generation. The reform supplied a phonetic Latin-based script that solved that standardization problem centrally.
% TRANSFER_FUNCTION: Moves cultural capital, scribal employment, and interpretive authority from the Ottoman-literate classes (jurists, clerks, calligraphers, endowed institutions) to a new Latin-literate state cadre and secular intelligentsia; simultaneously moves a millennium-scale textual inheritance out of general circulation and into specialist custody.
% ABSENT_VOICES: Continuity partisans, senior religious authorities, and experienced Ottoman-script educators were deliberately unseated from the Alphabet Commission's deliberations; diaspora communities still literate in Ottoman script, and the not-yet-born generations who would inherit the family-archive gap, had no seat at all. The unanimity of elite opinion around the reform was produced in a room from which every defender of the old script had been removed.
% DISAPPEARANCE_RATIONALE: If the rupture-doctrine and its enforcement machinery vanished overnight, schooling, bureaucracy, law, publishing, and street signage would all require re-coordination; the state's cadre pipeline, its archive regime, and its cultural legitimacy narrative are built on the Latin fact. Nothing snaps back — the Ottoman-script competence base has aged out — but every institution that produces or consumes official text would have to renegotiate its medium.
% FOUNDING_PROBLEM: A nation judged by its founders to be backward needed to break from a decadent imperial-confessional past and reach the level of contemporary (Western) civilization; the Arabic-derived script was diagnosed as both the emblem and the carrier of that past, and as a mechanical obstacle to mass learning and modern administration.
% FOUNDING_PROBLEM_CORROBORATION: Outside the beneficiary set: contemporary foreign diplomatic correspondence and press attested the backwardness diagnosis as sincerely held, and the rapid imitation wave across Muslim-majority modernizing states (Central Asia, the Caucasus) corroborates that the founding problem was widely recognized beyond Turkey. Against that, continuity partisans and later multiple-modernities scholarship corroborate the sincerity while disputing the diagnosis itself — citing Japan, China, and Korea, which industrialized without script rupture — so external attestation confirms the problem was believed, not that it was real.
narrative_ontology:disappearance_verdict(orthographic_legitimacy_kernel__modernist_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_legitimacy_kernel__modernist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_legitimacy_kernel__modernist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(orthographic_legitimacy_kernel__modernist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_legitimacy_kernel__modernist_reading, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_legitimacy_kernel__modernist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(orthographic_legitimacy_kernel__modernist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(orthographic_legitimacy_kernel__modernist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.74: the arrangement expropriated the cultural capital of an entire literate stratum in a single legislative act — vocations, libraries, guild markets, and institutional circulation — and converted it into the founding capital of a new cadre; the acute phase decayed as the dispossessed cohorts aged out, settling into a chronic toll (archive access gated behind specialist training, religious-textual knowledge pushed out of general literacy) that persists at interval end. Suppression 0.42 reflects the end-state: active coercion largely retired, but the structural closure persists — a single state curriculum monopoly, no official path to dual-script literacy, heritage access available only through scarce licensed channels. Accessibility_collapse 0.62: the official public sphere collapsed completely to Latin within a decade, but private, devotional, and diaspora channels preserved Arabic-letter competence, so alternatives narrowed severely without vanishing. Resistance 0.58: real and recurring — conservative parliamentary opposition, passive non-compliance in religious education, diaspora publishing, periodic restoration proposals — never sufficient to reopen the public sphere. Theater_ratio 0.38 and rising: the early campaign was intensely functional (night schools, mass examinations); over time commemoration, anniversary ritual, and textbook enshrinement have grown as shares of activity while the living enforcement function shrank — Goodhart drift visible but still below the 0.5 line.
 *   
 *   Identity-lock dynamics (ulema seat): the binding mechanism is triple — professional (credentials invalid, retraining means starting over), relational (community standing and discipleship chains constituted in the old script), and ideological (the Arabic letterform carries revealedness; transliteration feels like desecration). Exit was therefore not merely costly but self-dissolving. Had the identity frame broken — as it partially did for a minority who retrained — the payer seat would compute nearer the mobile end and the constraint's hold would weaken accordingly.
 *   
 *   Coalition consideration: the victim seats shared a common interest and briefly a common cause with continuity_partisans, but the coalition was foreclosed preemptively — the one-party state removed the opposition from the room before the rule hardened. Post-1946 multiparty competition partially re-empowered religious conservatism (restored religious education, softened laicism) yet never reopened the script question; the coalition potential existed structurally and was defeated temporally, which is itself diagnostic of how much enforcement the arrangement consumed early.
 *   
 *   Coordination type: identity_coordination is declared because under THIS reading the dominant function whose failure would defeat the arrangement is the marking of civilizational alignment — the script is the nation's identity boundary against its own past. The FNL gaming caution applies with force: identity framing ('this is who we are now') is precisely the cover under which asymmetric coupling hides. Here the coupling concentrates burdens on moderate-power literate classes at national-to-continental scope while the payoff accrues to the state center; the offset accommodates genuine boundary-maintenance complexity, it does not launder that asymmetry, and the engine's Power x Scope check should be read against it.
 *   
 *   Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, in the engine's computation. Claim and metrics are independent authored facts: I claim tangled_rope because both a genuine coordination function and asymmetric extraction are structurally present, while the metrics describe heavily extractive, formerly coercive operation — the divergence between the computed per-seat classifications and this claim is the datum, not an error to reconcile.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently and the structural data explains why. From the agenda-setter seat the arrangement is a founding act of national rebirth: the apparatus wrote it, staffed it, and collects its dividend, so its computed type leans rope-like regardless of the aggregate metrics. From the identity_locked payer seat the identical structure operates as confiscation — a lifetime of textual capital nullified by decree, exit equivalent to self-abolition — computing toward the snare end. The trapped clerk and craftsman seats sit between: real losses, but losses they can narrate as historical misfortune rather than existential theft, so their computed severity discounts with time-horizon. The rural seat splits internally: as beneficiary-of-record (literacy delivered) versus bearer of the household archive-gap, its two roles pull the computed classification in opposite directions, and the engine's per-seat output should show that tension rather than average it away. The excluded continuity seat experiences the arrangement at maximum severity — total severance — because it evaluates the same facts under the rival reading's criterion; that evaluation belongs to the sibling file, not this one. The observer seats see the whole distribution and collect nothing.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. The apparatus and intelligentsia sit near the beneficiary pole (low d): the arrangement subsidizes their authority and careers; the apparatus additionally holds arbitrage-grade exit since it occupies the rule-writing position. The four victim groups sit near the full-target pole (high d), amplified by exit condition — identity_locked for the ulema, trapped for clerks, craftsmen, and endowments — since immobile targets bear the full weight where mobile ones can reroute. The rural population declares as beneficiary and will derive low-to-moderate d accordingly; my qualitative judgment is that their realized position is near-symmetric (literacy gained, ancestral textuality lost, transition costs borne), so the engine's derived d for that seat is expected to sit below my descriptive estimate — that gap is recorded as signal in the omegas and commentary, not corrected by override, because overriding on a coarse power atom would drag the genuinely targeted powerless craftsmen seat with it. The excluded continuity seat lies outside the beneficiary/victim arrays: the arrangement is imposed on it without representation, which the derivation registers through its constrained exit rather than through d.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification work this story performs is keeping the two failure modes apart. Read through extraction alone, the reform is a pure dispossession — an elite liquidated by decree — which erases the genuine collective good (a teachable national script, mass literacy infrastructure, technological compatibility) that millions undeniably received. Read through coordination alone, it is a triumphant modernization — which erases the deliberate severance of a civilization's textual memory and the mid-career destruction of thousands. Tangled_rope holds both: coordination function real, extraction asymmetric and partly constitutive. The R5 genealogy interview sharpens this: founding_problem_status is contested (the modernist seat still frames catch-up-to-the-West as live; the continuity and multiple-modernities seats hold the diagnosis mistaken), paired with disappearance_verdict world_rearranges — no dead-mandate mismatch flag fires, correctly, because the arrangement still organizes the world. The monitored risk is forward-looking: theater_ratio's steady climb (0.12 -> 0.38) tracks the migration of the arrangement's living function into commemoration; if the rupture premise were fully repudiated (drift_state moving from substantial to severe), the residue would be performance without constituency — the piton trajectory — and the temporal series here gives the drift detector the baseline to date that transition if it comes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'This file instantiates one reading (modernist_reading) of the orthographic_legitimacy_kernel; would instantiating a sibling reading restructure the constraint''s parties, directionality map, and epsilon?',
    'Author the sibling files (continuity_reading, instrumentalist_reading) and compare their beneficiary/victim sets and epsilon values against this file''s; convergence on structure despite divergent valence confirms the kernel is stable and the disagreement is located where this file places it — in the legitimacy criterion, not in the facts.',
    'Under continuity_reading the same Latin arrangement appears as maximal severance (epsilon rises, the victim set expands toward all tradition-bearers); under instrumentalist_reading it appears as efficiency delivery (epsilon falls, rural seats migrate toward pure beneficiary). The classification of the Latin arrangement is reading-relative by design; the cross-reading comparison is the meta-analytic product, not noise to be eliminated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Committer-frame omega: this constraint is one reading of a contested kernel; sibling readings instantiate different constraints over the same facts.').

omega_variable(
    constitutive_extraction_ambiguity,
    'Was the dispossession of the traditional literate classes a purpose of the reform (identity transformation requires discrediting the old carriers of textual authority) or an incidental side-effect (an efficiency reform that happened to strand them)?',
    'Comparative analysis of Latinization episodes with differing stated rationales: identity-driven transitions (Turkey 1928, Turkmenistan) versus stalled or purely administrative ones (Uzbekistan''s incomplete transition); if dispossession depth tracks identity rhetoric rather than administrative need, the constitutive reading wins.',
    'If constitutive, the extraction is load-bearing for the arrangement''s function and the constraint leans snare-ward despite its coordination goods; if incidental, the tangled_rope reading stands with a recoverable coordination core and the extraction is a regrettable transition cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutive_extraction_ambiguity, conceptual, 'Whether elite dispossession was the point of the script change or its byproduct.').

omega_variable(
    latinization_literacy_counterfactual,
    'Would literacy and administrative modernization have risen comparably under a reformed-but-non-Latin orthography — the simplified Arabic-based schemes debated through the 1920s and partially implemented elsewhere?',
    'Econometric comparison across reform trajectories holding schooling investment approximately constant (Turkey versus Egypt, Iran, and the Soviet Central Asian republics), isolating the script variable from the education-spending variable.',
    'If comparable gains were available without rupture, the coordination good did not require the extraction — the asymmetry was chosen, not necessitated, pushing the classification snare-ward. If the Latin switch bought measurably more literacy per unit of schooling investment, part of the measured extraction is the genuine price of the coordination good, stabilizing the tangled_rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(latinization_literacy_counterfactual, empirical, 'Counterfactual test of whether the rupture premium purchased the literacy gains.').

omega_variable(
    heritage_access_reversibility,
    'Does the chronic extraction component — the archive-access toll and the exclusion of religious-textual knowledge from general literacy — persist regardless of digitization, or does large-scale OCR and transliteration of Ottoman-script corpora dissolve it?',
    'Track usage rates of digitized Ottoman collections against specialist-mediated access over time; if machine-readable access substitutes for trained readership, the toll is technological and temporary.',
    'Reversibility would decay the payer seats'' long-run burden and pull late-interval effective extraction down; persistence entrenches the trapped/identity_locked seats'' classification and supports treating the chronic toll as a permanent structural feature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(heritage_access_reversibility, empirical, 'Whether the heritage-access component of extraction is technologically reversible.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_legitimacy_kernel__modernist_reading, 1928, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orth_tr_t1928, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 1928, 0.12).
narrative_ontology:measurement(orth_tr_t1950, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 1950, 0.18).
narrative_ontology:measurement(orth_tr_t1980, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 1980, 0.26).
narrative_ontology:measurement(orth_tr_t2000, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 2000, 0.32).
narrative_ontology:measurement(orth_tr_t2025, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 2025, 0.38).

% Extraction over time
narrative_ontology:measurement(orth_be_t1928, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 1928, 0.82).
narrative_ontology:measurement(orth_be_t1950, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 1950, 0.8).
narrative_ontology:measurement(orth_be_t1980, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 1980, 0.77).
narrative_ontology:measurement(orth_be_t2000, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 2000, 0.75).
narrative_ontology:measurement(orth_be_t2025, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 2025, 0.74).

% Suppression requirement over time
narrative_ontology:measurement(orth_su_t1928, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 1928, 0.85).
narrative_ontology:measurement(orth_su_t1950, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 1950, 0.68).
narrative_ontology:measurement(orth_su_t1980, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 1980, 0.52).
narrative_ontology:measurement(orth_su_t2000, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 2000, 0.46).
narrative_ontology:measurement(orth_su_t2025, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 2025, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_legitimacy_kernel__modernist_reading, identity_coordination).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__modernist_reading, orthographic_legitimacy_kernel__continuity_reading).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__modernist_reading, orthographic_legitimacy_kernel__instrumentalist_reading).

% DUAL FORMULATION NOTE:
% Constraint family: 'alphabet reform' decomposes into three structurally distinct constraints under the epsilon-invariance principle, because the colloquial label conflates three legitimacy criteria that yield different epsilon values over the same Latin fact pattern. This file instantiates the modernist reading (legitimacy = civilizational alignment + rupture; epsilon ~= 0.74, extraction constitutive of identity transformation). The continuity sibling authors maximal severance-loss over the same arrangement; the instrumentalist sibling authors efficiency-delivery with rural seats shifted toward the beneficiary pole. Upstream-downstream structure: the modernist instantiation is upstream — its achievement created the fact pattern the instrumentalist defense now operates on, and its rupture premise is what the continuity reading exists to contest. Each file links both siblings via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
