% ============================================================================
% CONSTRAINT STORY: correct_latin_kernel_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin_kernel_flat_control, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:flat_control_of/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: correct_latin_kernel_flat_control
 *   human_readable: Correct Latin as Normative Standard
 *   domain: historical_linguistics/intellectual_history
 *
 * SUMMARY:
 *   The 'correct Latin' standard represents a stabilized commitment to a
 *   normative linguistic authority claimed as continuous with classical
 *   antiquity. Both medieval ecclesiastical practitioners and Renaissance
 *   humanist scholars invoked classical Latin as the legitimate standard, yet
 *   their interpretations diverged significantly: medieval scribes worked
 *   within a living tradition of usage-based norms, while humanists
 *   reconstructed classical forms often at variance with medieval practice.
 *   The constraint functions simultaneously as a genuine coordination
 *   mechanism (enabling inter-regional clerical communication, facilitating
 *   scholarly exchange) and as an extraction mechanism (gatekeeping literacy,
 *   suppressing vernacular languages, imposing standards that are
 *   historically contingent yet presented as absolute). The temporal
 *   trajectory shows rising extractiveness through the high medieval period
 *   (as the standard hardened into explicit rules), peaking at the late
 *   medieval period (when humanist reconstruction created maximum tension
 *   between medieval usage and classical forms), then stabilizing in the
 *   early modern period as the theater ratio rises—suggesting the constraint
 *   increasingly operates through performative ritual rather than functional
 *   necessity.
 *
 * KEY AGENTS:
 *   - Working Scribes: Powerless/trapped victims — bear enforcement burden of incoherent standard. Face constant correction and rework for failing to meet standards defined retrospectively.
 *   - Vernacular Language Communities: Powerless/trapped victims — linguistic systems emerging from medieval Latin but systematically delegitimized as corrupt. Trapped in subordinate status across generations.
 *   - Clerical Authority (Monastic/Ecclesiastical Institutions): Institutional/constrained beneficiaries — genuinely coordinate ecclesiastical communication while extracting gatekeeping benefits. Cannot easily exit without institutional reorganization.
 *   - Humanist Intellectuals: Institutional/arbitrage beneficiaries — see constraint as pure coordination (shared textual standards enabling scholarship). Net beneficiaries with exit options (participate in republic of letters or operate locally).
 *   - Crown and Secular Elites: Powerful/constrained actors — benefit from literacy infrastructure but constrained by dependence on clerical and humanist gatekeepers. Incentivized to develop vernacular alternatives.
 *   - The Constraint Apparatus: Institutional/arbitrage—by early modern period, maintained primarily through institutional momentum and prestige rather than functional necessity (piton perspective).
 *   - Analytical Observer: Civilizational perspective risks naturalizing the choice to privilege classical forms as inevitable rather than contingent.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin_kernel_flat_control, 0.38).
domain_priors:suppression_score(correct_latin_kernel_flat_control, 0.62).
domain_priors:theater_ratio(correct_latin_kernel_flat_control, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin_kernel_flat_control, extractiveness, 0.38).
narrative_ontology:constraint_metric(correct_latin_kernel_flat_control, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(correct_latin_kernel_flat_control, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin_kernel_flat_control, tangled_rope).
narrative_ontology:human_readable(correct_latin_kernel_flat_control, "Correct Latin as Normative Standard").
narrative_ontology:topic_domain(correct_latin_kernel_flat_control, "historical_linguistics/intellectual_history").

domain_priors:requires_active_enforcement(correct_latin_kernel_flat_control).

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(correct_latin_kernel_flat_control, correct_latin_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin_kernel_flat_control, clerical_authority).
narrative_ontology:constraint_beneficiary(correct_latin_kernel_flat_control, humanist_intellectuals).
narrative_ontology:constraint_victim(correct_latin_kernel_flat_control, vernacular_languages).
narrative_ontology:constraint_victim(correct_latin_kernel_flat_control, working_scribes).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(correct_latin_kernel_flat_control, clergy_and_ecclesiastical_institutions).
narrative_ontology:constraint_beneficiary(correct_latin_kernel_flat_control, humanist_scholars).
narrative_ontology:constraint_beneficiary(correct_latin_kernel_flat_control, secular_rulers_and_nobles).
narrative_ontology:constraint_beneficiary(correct_latin_kernel_flat_control, universities_and_intellectual_institutions).
narrative_ontology:constraint_victim(correct_latin_kernel_flat_control, monastic_and_clerical_scribes).
narrative_ontology:constraint_victim(correct_latin_kernel_flat_control, romance_and_germanic_speakers).
narrative_ontology:constraint_victim(correct_latin_kernel_flat_control, secular_rulers_and_nobles).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Scribes copy manuscripts and administrative documents in monastic scriptoria and chanceries. They are instructed to write 'correctly' according to standards that are often unclear (medieval usage vs classical reconstruction), inconsistently enforced (varying standards by institution), and constantly evolving (as humanist models spread). Correction of their work is frequent; standards for correctness are presented as absolute but prove incoherent when applied. They cannot refuse copying work without losing livelihood and social position. The constraint extracts their labor through endless rework and implicit blame for failures to meet an incoherent standard.
narrative_ontology:constraint_stakeholder(correct_latin_kernel_flat_control, monastic_and_clerical_scribes, payer,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(correct_latin_kernel_flat_control, monastic_and_clerical_scribes, agenda_setter).

% Monastic, cathedral, and papal authorities use the Latin correctness standard as a mechanism for internal hierarchy (clergy vs lay), gatekeeping of literacy access, and control over textual interpretation (only the correct reading of sacred texts is permitted). The standard genuinely serves coordination functions: clerical communication across dioceses and regions requires some unified linguistic practice; the authority of the Church rests partly on its claim to control correct doctrine, which extends to correct language. But the constraint also extracts through exclusivity: non-clergy are systematically barred from literacy instruction; clergy control who can read sacred texts and how they are interpreted. The institution benefits from the gatekeeping function but cannot easily exit without reorganizing its hierarchy and authority structure.
narrative_ontology:constraint_stakeholder(correct_latin_kernel_flat_control, clergy_and_ecclesiastical_institutions, beneficiary,
    institutional, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(correct_latin_kernel_flat_control, clergy_and_ecclesiastical_institutions, agenda_setter).

% Renaissance humanist intellectuals treat 'correct Latin' as reconstructed classical forms—the language of Cicero and Virgil—as the standard for learned communication. This standard aligns perfectly with their professional practice (reading, imitating, commenting on classical texts) and their social positioning (intellectual elites claiming connection to ancient authority). Humanist scholars can exit by operating locally or in vernacular, but they choose to participate because the shared Latin standard enables the international republic of letters. They experience the constraint as pure coordination—shared standards enable collaboration, textual comparison, and prestige. The constraint serves their interests so directly that they become its strongest advocates and enforcement mechanism.
narrative_ontology:constraint_stakeholder(correct_latin_kernel_flat_control, humanist_scholars, beneficiary,
    institutional, immediate, arbitrage, global).

% Communities speaking Romance languages (Occitan, Old French, Tuscan, Castilian) and Germanic languages emerge naturally from medieval Latin usage, but they are systematically treated as corrupt deviations from correct Latin rather than as legitimate linguistic evolution. The correctness standard delegitimizes these languages as 'vulgar tongues' unfit for serious intellectual work or administrative authority. Speakers cannot exit this status hierarchy: access to education, prestige, administrative positions, and intellectual authority requires Latin mastery. Over generations, the constraint suppresses the legitimacy of emerging national languages and delays their adoption for official purposes. The extraction is structural—trapped in subordinate status without individual exit options.
narrative_ontology:constraint_stakeholder(correct_latin_kernel_flat_control, romance_and_germanic_speakers, payer,
    powerless, generational, trapped, continental).

% Kings, princes, and noble administrators benefit from the Latin correctness infrastructure—it provides educated administrators, literacy gatekeeping that supports hierarchy, and connection to Church authority and classical prestige. But they are constrained by dependence on clerical and humanist expertise for Latin instruction and textual interpretation. As their own administrative apparatuses grow, secular rulers face incentives to develop literacy in their own vernaculars (French for French kings, Spanish for Castilian rulers) rather than depend on clerical intermediaries. The constraint benefits them through the literacy infrastructure but costs them through institutional dependence.
narrative_ontology:constraint_stakeholder(correct_latin_kernel_flat_control, secular_rulers_and_nobles, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(correct_latin_kernel_flat_control, secular_rulers_and_nobles, payer).

% Universities and learned institutions (colleges, academies, printing establishments) institutionalize the Latin correctness standard through formal curricula, prestigious degrees, and control over publication. By the early modern period, this apparatus becomes self-reinforcing: to be educated is to read Latin correctly; to be a scholar is to write in correct Latin; to publish is to follow Latin standards. The institution has arbitrage exit available (could switch to vernaculars as communication languages; in fact, universities eventually do this) but maintains Latin standards through prestige and credentialing. The constraint becomes increasingly performative—Latin correctness is taught and practiced because it is the mark of education, not because it is functionally necessary for coordination.
narrative_ontology:constraint_stakeholder(correct_latin_kernel_flat_control, universities_and_intellectual_institutions, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(correct_latin_kernel_flat_control, universities_and_intellectual_institutions, beneficiary).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enabling inter-regional communication among clergy, scholars, and administrators who would otherwise speak mutually unintelligible vernaculars. Providing a shared textual foundation for theological and intellectual authority. Facilitating the interpretation and transmission of sacred texts (Scripture, Church fathers) across linguistic communities.
% TRANSFER_FUNCTION: Status and prestige flow from non-Latin speakers to Latin speakers and from Latin speakers with low correctness to Latin speakers with high correctness. Administrative authority flows through Latin-literate intermediaries (clergy, educated scribes) rather than through direct vernacular authority. Intellectual authority concentrates with those who control correct interpretation of Latin texts.
% ABSENT_VOICES: The vernacular speakers whose languages are emerging from medieval Latin usage are excluded from the conversation that defines them as corrupt. No formal representation exists for scribal labor interests—the standard is imposed downward without participation of those bearing its costs. Regional and local scribal traditions, which may have sound reasons for variant usages, are not consulted in standard-setting.
% DISAPPEARANCE_RATIONALE: If the 'correct Latin' standard disappeared, clerical administration would shift toward vernacular literacy (as it actually does by the 16th-18th centuries). International scholarship would fragment or adopt whatever new lingua franca emerged (as it does adopt English by the 20th century). Ecclesiastical authority would lose one mechanism of gatekeeping and hierarchy maintenance. The organizational structure of universities, publishing, and intellectual institutions would need to reorganize around whatever languages they adopted. The constraint's disappearance would not leave the world unchanged—it would require major institutional and linguistic restructuring.
% FOUNDING_PROBLEM: In the early medieval period (roughly 6th-8th centuries), after the fragmentation of the Roman Empire and the emergence of distinct Romance and Germanic languages, there was a genuine coordination problem: How do clergy, scholars, and administrators across mutually unintelligible linguistic regions communicate? How is ecclesiastical authority maintained across linguistic divides? Latin provided an inherited, prestigious solution to this problem—all educated clergy already knew it; it connected to imperial and classical authority; it was neutral with respect to emerging vernacular rivalries.
% FOUNDING_PROBLEM_CORROBORATION: By the early modern period (16th century onward), the founding coordination problem is documented as solved by alternative mechanisms: vernacular literacy becomes widespread; vernacular languages become prestigious for administration and intellectual work; printing in vernaculars accelerates; universities begin teaching in vernaculars; diplomatic and commercial communication happens in French, Spanish, English rather than Latin. The problem is not that Latin disappeared—it persisted in universities and scholarly communities—but that it ceased to be functionally necessary for the main coordination problems it originally solved. Multiple historical sources document the shift: rulers increasingly appoint administrators literate in vernaculars; printers begin publishing in vernaculars at scale; scholars publish in vernaculars while Latin becomes ornamental. The constraint persists but the founding mandate no longer drives its persistence.
narrative_ontology:disappearance_verdict(correct_latin_kernel_flat_control, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin_kernel_flat_control, dead).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE WORKING SCRIBE (SNARE) — A scribe copying documents in a monastery or chancery faces impossible enforcement of correctness standards that are historically contingent (medieval usage vs reconstructed classical forms) yet presented as absolute. Exit is material—cannot refuse copying work and retain livelihood. Extraction comes through constant correction, rework, and implicit blame for failures to meet an incoherent standard. Bears full cost; collects no benefit.
constraint_indexing:constraint_classification(correct_latin_kernel_flat_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: VERNACULAR LANGUAGE COMMUNITIES (SNARE) — Romance and Germanic vernaculars emerge from medieval Latin usage but are systematically delegitimized by the 'correctness' standard, which treats them as corruption rather than natural evolution. Communities cannot exit the status hierarchy: Latin correctness becomes a gate for literacy, learning, advancement. Vernaculars are trapped in subordinate status across generations. No coordination function for vernaculars themselves—only extraction through linguistic subordination.
constraint_indexing:constraint_classification(correct_latin_kernel_flat_control, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 3: THE CLERICAL AUTHORITY (TANGLED ROPE) — Monastic and ecclesiastical institutions benefit from Latin correctness as a gate-keeping mechanism: literacy control, internal hierarchy maintenance, exclusive access to sacred texts. But they also genuinely coordinate ecclesiastical communication across Christendom—Latin does solve the real problem of inter-regional clerical communication. Constrained exit: shifting away from Latin correctness would require institutional reorganization; the constraint produces both coordination gains and extraction surplus for the institution.
constraint_indexing:constraint_classification(correct_latin_kernel_flat_control, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 4: THE HUMANIST INTELLECTUALS (ROPE) — Humanist scholars treating 'correct Latin' as reconstructed classical forms experience the constraint as a coordination mechanism: shared textual standards enable collaborative scholarship, textual comparison, and literary community across regions. Arbitrage exit available (use Latin correctly and participate in the international republic of letters; reject it and operate locally). Net beneficiaries—the constraint is transparent to their interests because correctness is defined as classical mastery, which is their core professional activity.
constraint_indexing:constraint_classification(correct_latin_kernel_flat_control, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: THE CROWN AND SECULAR ELITES (TANGLED_ROPE) — Secular rulers need Latin-literate administrators but also begin to value vernacular administrators (French, Spanish, English speakers). Constrained by the linguistic infrastructure already built around Latin but incentivized to develop vernacular administration. Extraction comes through dependence on clerical and humanist gatekeepers; coordination comes through shared Latin administrative standards. Mixed experience: benefit from literacy infrastructure but pay through institutional dependence.
constraint_indexing:constraint_classification(correct_latin_kernel_flat_control, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: THE CONSTRAINT APPARATUS (PITON) — By the 17th century, the 'correct Latin' standard persists primarily as performative ritual. Universities teach it; texts are published in it; scholarship uses it. But the real coordination work (international trade, diplomacy, administration) increasingly happens in vernaculars. The theater ratio is high: the constraint maintains itself through institutional momentum, prestige markers, and exclusionary credentials rather than through functional necessity. The apparatus endures as inertia.
constraint_indexing:constraint_classification(correct_latin_kernel_flat_control, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: THE ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a civilizational distance, correctness in any language is necessarily normative and contested: variation is inherent to living language use. The classical/medieval distinction is itself a historical artifact. The view risks naturalizing what is actually a contested institutional commitment: that medieval practitioners were corrupt and humanists restored truth. The analytical observer sees an immutable feature of language (all languages have variation; all have norming standards) and mistakes a contingent choice (which variant to privilege) for a natural law.
constraint_indexing:constraint_classification(correct_latin_kernel_flat_control, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin_kernel_flat_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(correct_latin_kernel_flat_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(correct_latin_kernel_flat_control, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(correct_latin_kernel_flat_control, TR),
    TR >= 0.70.

:- end_tests(correct_latin_kernel_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   EXTRACTIVENESS (0.38): Moderate. The constraint extracts through gatekeeping (controlling who is literate, who can hold administrative positions), through suppressing vernacular legitimacy, and through imposing standards on scribal labor that are historically contingent (medieval usage vs humanist reconstruction) yet presented as absolute. However, extractiveness is not higher (0.60+) because the constraint does coordinate real functions: clerical communication across regions, scholarly exchange, administrative documentation. The extract benefit flows primarily to clerical institutions and humanist intellectuals; the costs fall on scribes and vernacular communities. The temporal trajectory shows rising extractiveness through the medieval period as the standard hardened into explicit rules (0.28→0.42), then slight decline in early modern period (0.38) as functional necessity decreases and vernacular alternatives emerge. SUPPRESSION (0.62): Moderate-high. Substantial barriers to exit include: material dependence on clerical positions (for scribes), status hierarchy treating vernaculars as inferior (for language communities), career gatekeeping requiring Latin mastery, and the prestige/exclusivity system built around correctness. However, suppression is not total—some scribes can ignore the standard (with career consequences); some regions develop stronger vernacular traditions; by early modernity, competing literacy pathways emerge. THEATER_RATIO (0.58): Moderate-high and rising. Medieval practice shows lower theater ratio (0.35)—the standard was functional and negotiable, embedded in living practice. By early modern period (0.68), the theater ratio is high: universities teach 'correct Latin' in texts and exercises; prestigious scholarship publishes in it; correctness becomes a credential signal. But the coordination work increasingly happens in vernaculars. The rising trajectory indicates the constraint is increasingly performative—maintained through institutional ritual and prestige markers rather than through functional necessity. The mechanism shifts from 'we need this standard to communicate' to 'using this standard marks you as educated/authoritative.'
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits radical perspectival divergence rooted in structural position. The working scribe and vernacular communities see SNARE: incoherent standards imposed from above, constant enforcement, no benefit, no exit. The clerical authority sees TANGLED_ROPE: genuine coordination (ecclesiastical communication requires unified standards) mixed with extraction benefits (gatekeeping, hierarchy maintenance). The humanist sees ROPE: pure coordination (shared classical standards enable scholarship). The secular elite sees TANGLED_ROPE from a different angle: benefit from literacy infrastructure but constrained by dependence on clerical/humanist gatekeepers. The apparatus itself has become PITON: maintained through institutional momentum, prestige, and credentials rather than functional necessity. The analytical observer risks seeing MOUNTAIN (naturalizing the choice as inevitable) and would be wrong—the false summit detector should flag this. The perspectival gaps arise from genuine structural differences: agents with arbitrage exit (humanists) experience the constraint as beneficial coordination; agents with trapped exit (scribes) experience pure extraction; institutional beneficiaries (clergy) experience mixed coordination/extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality and effective extraction are computed by the engine from these structural declarations. I author the beneficiary/victim declarations (which agents collect from the constraint, which bear costs) and the exit options (can the agent walk away and on what terms). The engine computes d and chi. Working scribes are victims with trapped exit → high d → high chi. Clerical institutions are beneficiaries with constrained exit → moderate d → moderate chi. Humanist scholars are beneficiaries with arbitrage exit → low d → low/negative chi. The perspectival gaps arise because d values differ radically across agent positions—the scribe and humanist experience the same constraint completely differently because their structural positions produce opposite d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The 'correct Latin' constraint faces a potential mandatrophy: the founding mandate appears to have been to enable inter-regional clerical and scholarly communication through a shared standard. That mandate was live in the early medieval period (0.28 extractiveness) when Latin was genuinely the lingua franca and variation in usage caused real coordination problems. By the high medieval period, the mandate was increasingly hardened into explicit rules (extractiveness rises to 0.42). By the late medieval period, tension emerges between medieval Latin usage and humanist classical reconstruction (extractiveness peaks at 0.42). By early modernity, the functional necessity has declined sharply (extraction falls to 0.38) while theater ratio rises (0.68)—indicating the constraint now persists primarily through institutional momentum and prestige rather than functional necessity. The mandate is potentially dead or dying: vernacular languages increasingly handle real coordination work (trade, diplomacy, administration); Latin correctness becomes a credential signal rather than a communication requirement. However, mandatrophy is not fully resolved: universities still teach it; prestigious scholarship still uses it; the constraint still suppresses vernacular legitimacy. The story suggests we are in a period of mandate obsolescence without institutional collapse—the constraint is shifting from rope (functional coordination) toward piton (inertial performance). A full mandatrophy resolution would require documenting whether the institutional actors (universities, humanist communities, clergy) recognize the mandate as dead or continue to treat it as live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    classical_vs_medieval_authenticity,
    'Was medieval Latin a genuine continuation of classical Latin, or a corruption of it? Or is the distinction between continuity and corruption itself a humanist-era reframing?',
    'Historical textual analysis of continuity patterns; examination of whether medieval practitioners saw themselves as continuing or deviating; investigation of whether humanists'' reconstruction of classical norms was accurate or idealized',
    'If medieval practitioners were genuinely continuing classical tradition: the constraint is rope (genuine coordination of inherited standard). If medieval Latin was a separate system: the constraint is tangled_rope (humanists impose alien standard on medieval materials). If both views are perspectival: the constraint naturalizes one choice and suppresses the other—higher extractiveness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(classical_vs_medieval_authenticity, empirical, 'Whether medieval Latin was a continuation of or corruption of classical Latin').

omega_variable(
    humanist_reconstruction_accuracy,
    'Did humanist scholars accurately recover classical Latin norms, or did they construct an idealized classical standard that served humanist intellectual positioning?',
    'Comparison of humanist reconstructed rules against actual classical texts; analysis of which classical texts humanists cited vs ignored; investigation of whether reconstructed norms were internally consistent or served rhetorical purposes',
    'If accurate: humanists enable genuine coordination around a stable standard (rope/scaffold). If constructed: humanists impose extraction through false authority (tangled_rope with higher extraction). If mixed: the constraint benefits from false universality (false summit candidate).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(humanist_reconstruction_accuracy, empirical, 'Whether humanist classical reconstruction was accurate or idealized').

omega_variable(
    functional_necessity_of_latin_standard,
    'Did medieval and early modern societies require a unified Latin standard for the coordination functions they used Latin for, or would local variation have served those functions equally well?',
    'Historical documentation of coordination breakdowns due to linguistic variation; comparison of regions with stricter vs looser Latin standards; analysis of whether vernacular languages could have substituted earlier',
    'If necessary: the constraint is rope/tangled_rope with genuine coordination function. If unnecessary: the constraint is snare/piton maintained by prestige and gatekeeping rather than functional requirement. Directly affects mandate resolution—if functional necessity is gone, mandatrophy is present.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_necessity_of_latin_standard, empirical, 'Whether unified Latin standard was functionally necessary').

omega_variable(
    enforcement_mechanism_visibility,
    'How much of the suppression (0.62) comes from explicit institutional enforcement (threat of punishment, exclusion from positions) versus internalized norm acceptance (agents voluntarily adopt the standard as legitimate)?',
    'Documentation of formal penalties for incorrect Latin; analysis of linguistic change in contexts without formal enforcement; investigation of whether agents resisted or internalized the standard',
    'If primarily explicit: suppression would diminish quickly if enforcement ceased (lower true suppression). If internalized: suppression persists even after institutional enforcement declines (true suppression is higher). Affects exit_options classification—internalized suppression makes even constrained agents behave as if trapped.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_visibility, empirical, 'Whether suppression is explicit enforcement or internalized norm acceptance').

omega_variable(
    mandatrophy_status,
    'Is the founding mandate of the ''correct Latin'' standard—whatever that mandate was—still live or dead? Did the standard solve a problem that still exists, or was the problem situational and now resolved?',
    'Historical identification of the founding problem the standard addressed; assessment of whether that problem still exists in contemporary contexts; examination of whether persistence is functional or inertial',
    'If founding mandate is live: classification as rope/tangled_rope is correct (serves genuine function). If dead: classification should shift toward piton (maintained by inertia, not function). Directly affects narrative coherence—whether the constraint is explained by its benefits or by path-dependence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mandatrophy_status, conceptual, 'Whether the founding mandate of Latin correctness is still live or dead').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin_kernel_flat_control, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clatin_theater_t0_early_medieval, correct_latin_kernel_flat_control, theater_ratio, 0, 0.35).
narrative_ontology:measurement(clatin_theater_t3_high_medieval, correct_latin_kernel_flat_control, theater_ratio, 3, 0.45).
narrative_ontology:measurement(clatin_theater_t6_late_medieval, correct_latin_kernel_flat_control, theater_ratio, 6, 0.58).
narrative_ontology:measurement(clatin_theater_t9_early_modern, correct_latin_kernel_flat_control, theater_ratio, 9, 0.68).

% Extraction over time
narrative_ontology:measurement(clatin_extract_t0_early_medieval, correct_latin_kernel_flat_control, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(clatin_extract_t3_high_medieval, correct_latin_kernel_flat_control, base_extractiveness, 3, 0.35).
narrative_ontology:measurement(clatin_extract_t6_late_medieval, correct_latin_kernel_flat_control, base_extractiveness, 6, 0.42).
narrative_ontology:measurement(clatin_extract_t9_early_modern, correct_latin_kernel_flat_control, base_extractiveness, 9, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(clatin_suppress_t0_early_medieval, correct_latin_kernel_flat_control, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(clatin_suppress_t3_high_medieval, correct_latin_kernel_flat_control, suppression_requirement, 3, 0.58).
narrative_ontology:measurement(clatin_suppress_t6_late_medieval, correct_latin_kernel_flat_control, suppression_requirement, 6, 0.65).
narrative_ontology:measurement(clatin_suppress_t9_early_modern, correct_latin_kernel_flat_control, suppression_requirement, 9, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin_kernel_flat_control, information_standard).
narrative_ontology:affects_constraint(correct_latin_kernel_flat_control, vernacular_language_suppression).
narrative_ontology:affects_constraint(correct_latin_kernel_flat_control, clerical_literacy_gatekeeping).
narrative_ontology:affects_constraint(correct_latin_kernel_flat_control, humanist_textual_authority).

% DUAL FORMULATION NOTE:
% The 'correct Latin' constraint is the shared kernel across multiple institutional domains. Downstream constraints include the suppression of vernacular languages (their exclusion from prestige literacy), clerical gatekeeping of administrative literacy, and humanist authority over textual interpretation. Each downstream constraint has its own extractiveness profile; the shared kernel has its own. The network encodes these dependencies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
