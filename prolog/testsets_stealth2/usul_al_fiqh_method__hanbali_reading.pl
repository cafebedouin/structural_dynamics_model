% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__hanbali_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usul_al_fiqh_method__hanbali_reading, []).

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
 *   constraint_id: usul_al_fiqh_method__hanbali_reading
 *   human_readable: Hanbali Textual-Maximalist Derivation Regime
 *   domain: religious/legal-theoretical/comparative-law
 *
 * SUMMARY:
 *   The Hanbali reading of Islamic legal methodology binds derivation
 *   maximally to the Quran and authenticated hadith: analogical reasoning
 *   (qiyas) is confined to cases of clear textual silence, a weak hadith is
 *   preferred over sound analogy, and the blocking-of-means doctrine (sadd
 *   al-dhara'i) prohibits otherwise lawful acts that probabilistically lead
 *   to sin, all in service of textual fidelity against innovation (bid'a).
 *   The regime genuinely coordinates — it gives a trans-regional community a
 *   verifiable standard of provenance — while asymmetrically extracting from
 *   rationalist and customary legal development, which is why the claimed
 *   type is tangled_rope rather than rope or snare. This story is one member
 *   of a four-story constraint family decomposing the colloquial label 'usul
 *   al-fiqh': the hanafi, maliki, and shafii readings are separate
 *   constraints with their own files, linked through
 *   network.affects_constraints. Epsilon here is authored for THIS reading's
 *   own regime BY ITS OWN LIGHTS (referent: the standing Hanbali arrangement,
 *   not any endorsed alternative): the reading concedes real foreclosed
 *   development — weak-hadith instability, disputed breadth of the blocking
 *   doctrine, strain in novel domains — which lands its self-assessed
 *   extraction mid-range and rising with global institutional diffusion; a
 *   rationalist observer would rate the same referent substantially higher,
 *   and the sibling stories will author their own values independently.
 *
 * KEY AGENTS:
 *   - hanbali_mujtahids: Agenda setter (institutional/identity_locked) — administers authentication, licenses qiyas, polices bid'a; collects authority rents
 *   - hadith_scholars: Primary beneficiary (organized/constrained) — authentication work is the economy of legitimacy
 *   - lay_textualist_believers: Secondary beneficiary and payer (organized/identity_locked) — receives provenance assurance, bears innovation-policing costs
 *   - rationalist_jurists: Primary target (moderate/mobile) — toolkit delegitimized inside the regime
 *   - customary_community_practitioners: Primary target (powerless/trapped) — inherited practices condemned as innovation
 *   - modern_reformers: Target (moderate/constrained) — expansive tools licensed only grudgingly
 *   - rival_madhhab_establishments: Excluded party (institutional/arbitrage) — affected by diffusion, outside the conversation
 *   - comparative_law_analysts: Analytical observer — sees the full four-reading structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__hanbali_reading, 0.52).
domain_priors:suppression_score(usul_al_fiqh_method__hanbali_reading, 0.58).
domain_priors:theater_ratio(usul_al_fiqh_method__hanbali_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__hanbali_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__hanbali_reading, "Hanbali Textual-Maximalist Derivation Regime").
narrative_ontology:topic_domain(usul_al_fiqh_method__hanbali_reading, "religious/legal-theoretical/comparative-law").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__hanbali_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__hanbali_reading, 'bba6cb6f-8182-428b-983f-05fb9c66d07a').
narrative_ontology:cs_kernel_codification('bba6cb6f-8182-428b-983f-05fb9c66d07a', fixed_text).
narrative_ontology:cs_authority_grounding('bba6cb6f-8182-428b-983f-05fb9c66d07a', lineage).
narrative_ontology:cs_interpretation_layer_present('bba6cb6f-8182-428b-983f-05fb9c66d07a').
narrative_ontology:cs_reading_relation('bba6cb6f-8182-428b-983f-05fb9c66d07a', usul_al_fiqh_method__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('bba6cb6f-8182-428b-983f-05fb9c66d07a', usul_al_fiqh_method__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('bba6cb6f-8182-428b-983f-05fb9c66d07a', usul_al_fiqh_method__shafii_reading, influences).
narrative_ontology:cs_axiom('bba6cb6f-8182-428b-983f-05fb9c66d07a', foundational, textual_sources_maximally_restrictive).
narrative_ontology:cs_axiom_status(textual_sources_maximally_restrictive, holdable).
narrative_ontology:cs_axiom_grounding('bba6cb6f-8182-428b-983f-05fb9c66d07a', textual_sources_maximally_restrictive, deontological).
narrative_ontology:cs_axiom('bba6cb6f-8182-428b-983f-05fb9c66d07a', foundational, weak_hadith_preferred_over_qiyas).
narrative_ontology:cs_axiom_status(weak_hadith_preferred_over_qiyas, holdable).
narrative_ontology:cs_axiom_grounding('bba6cb6f-8182-428b-983f-05fb9c66d07a', weak_hadith_preferred_over_qiyas, conventional).
narrative_ontology:cs_axiom('bba6cb6f-8182-428b-983f-05fb9c66d07a', secondary, sadd_al_dharai_preserves_fidelity).
narrative_ontology:cs_axiom_status(sadd_al_dharai_preserves_fidelity, holdable).
narrative_ontology:cs_axiom_grounding('bba6cb6f-8182-428b-983f-05fb9c66d07a', sadd_al_dharai_preserves_fidelity, instrumental).
narrative_ontology:cs_reference_frame('bba6cb6f-8182-428b-983f-05fb9c66d07a', salaf_textual_supremacy).
narrative_ontology:cs_drift_state('bba6cb6f-8182-428b-983f-05fb9c66d07a', contemporary_mass_diffusion_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bba6cb6f-8182-428b-983f-05fb9c66d07a', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__hanbali_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanbali_reading, hanbali_mujtahids).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanbali_reading, hadith_scholars).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanbali_reading, lay_textualist_believers).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, rationalist_jurists).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, customary_community_practitioners).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, modern_reformers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, lay_textualist_believers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Senior Hanbali jurists who administer the method: they grade hadith, determine when textual silence is 'clear,' license or condemn analogical extension, and issue the rulings that police innovation. Their scholarly authority is constituted by mastery of this exact procedure; abandoning it would dissolve the basis of their standing. They collect deference, endowment income, court appointments, and council seats.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, hanbali_mujtahids, agenda_setter,
    institutional, generational, identity_locked, global).

% Specialists in transmission criticism whose discipline the method places at the center of legal derivation. Every ruling's legitimacy runs through their authentication work, securing patronage, students, and institutional posts. Their skill set has little exchange value under an analogical-reasoning-centered regime, so their fortunes rise and fall with this method's dominance.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, hadith_scholars, beneficiary,
    organized, generational, constrained, global).

% Ordinary adherents who receive rulings certified as directly traceable to revelation, which supplies religious assurance and a sharp criterion for condemning innovation. They also bear the method's restrictions in daily life, since valued practices can be condemned as innovation or blocked as paths to sin, and leaving the textualist community would cost them family, congregation, and self-conception.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, lay_textualist_believers, beneficiary,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__hanbali_reading, lay_textualist_believers, payer).

% Jurists formed in dialectical theology, philosophy, and expansive analogical method. Inside Hanbali-governed institutions their tools are delegitimized as unsanctioned opinion; they teach and publish under suspicion or relocate to Hanafi, Maliki, or secular-academic settings where their training retains market value.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, rationalist_jurists, payer,
    moderate, biographical, mobile, continental).

% Communities whose inherited practices — devotional gatherings, local festivals, folk healing, customary family arrangements — lack textual anchoring and therefore stand condemned as innovation or blocked as routes to sin. The practices constitute their communal life; abandoning them dissolves inherited identity, while keeping them invites censure from the textualist establishment.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, customary_community_practitioners, payer,
    powerless, generational, trapped, regional).

% Contemporary jurists and intellectuals addressing novel domains such as Islamic finance, bioethics, and constitutional governance, who need expansive instruments of public interest and purposive reasoning that the method licenses only grudgingly. They work by straining textual extension or borrowing sibling-school instruments, paying reputational cost within textualist circles either way.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, modern_reformers, payer,
    moderate, biographical, constrained, global).

% Hanafi, Maliki, and Shafi'i institutions operating parallel source hierarchies. They sit outside the Hanbali method-setting conversation yet are materially affected by its diffusion, as endowments, students, and state appointments flow toward textualist institutions. They respond by cultivating their own jurisdictions and transnational networks.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, rival_madhhab_establishments, excluded,
    institutional, generational, arbitrage, global).

% Academic historians and legal theorists who map the four readings' divergent source hierarchies and their institutional carriers. They hold no stake in any reading's victory and can see the full structure, including how each school's self-description understates what its method forecloses.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, comparative_law_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(usul_al_fiqh_method__hanbali_reading, hanbali_mujtahids).
narrative_ontology:fixing_cost_class(usul_al_fiqh_method__hanbali_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the derivation-coordination problem for a trans-regional community of jurisprudence: rulings must be traceable to verifiable revelation rather than to each jurist's preference, so maximal textual restriction supplies a shared, auditable standard that bounds innovation and keeps law anchored across generations and regions.
% TRANSFER_FUNCTION: Moves interpretive authority from reason-wielding jurists to text-authentication specialists; moves adjudicative discretion away from local custom and juristic preference toward canonical sources; and moves legitimacy, endowments, students, and offices toward whoever controls authentication.
% ABSENT_VOICES: Customary practitioners whose practices stand condemned, rationalist theologians in the Mu'tazilite lineage, women subject to rulings derived under restrictive extension, and rival-school jurists are not seated in the method-setting conversation; from inside the frame their objections register only as further instances of the innovation the method exists to block.
% DISAPPEARANCE_RATIONALE: If the maximal-restriction regime vanished overnight, Hanbali-governed jurisdictions would re-derive law through analogical and public-interest instruments within a generation; the Salafi-textualist movement would lose its defining criterion for condemning innovation; authentication specialists would lose their central economic position; and customary practices currently suppressed as innovation would regain legal space — the whole textualist economy of authority rearranges.
% FOUNDING_PROBLEM: After the Prophet's death the community faced proliferating personal opinion offered as God's law, culminating in the mihna, a caliphal inquisition demanding doctrinal concession. Ibn Hanbal's refusal, and the method built upon it, answer the problem of distinguishing revelation from human invention under pressure: bind derivation to verified transmission so that reason cannot dress itself in divine authority.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: Hanafi and Maliki jurists historically conceded the danger of unregulated opinion even while rejecting Hanbali extremity, and their own methodological disciplines exist partly because that danger is real; the mihna episode is independently attested history showing the coercive problem textual steadfastness answered; and Western academic legal history documents the anti-rationalist impetus without reliance on Hanbali self-description.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__hanbali_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__hanbali_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__hanbali_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(usul_al_fiqh_method__hanbali_reading, 'none', 1).
narrative_ontology:epsilon_provenance(usul_al_fiqh_method__hanbali_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__hanbali_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(usul_al_fiqh_method__hanbali_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(usul_al_fiqh_method__hanbali_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.52 because the regime transfers interpretive authority to authentication specialists and forecloses development channels, while the reading's own lights discount much of what it 'takes' as never-legitimate innovation; the tradition itself concedes residual costs, and the temporal series shows extraction accumulating as the regime scales globally. Suppression is 0.58: real enforcement machinery (grading regimes, virtue-enforcement institutions in Najdi contexts, censure shading toward takfir rhetoric) bounded by the fact that sibling madhhabs persist as live alternatives. Theater is 0.28: authentication is genuine labor, but a growing share of activity is performative condemnation of innovation in mass and social media. Accessibility collapse is 0.62: inside the frame, alternatives collapse nearly completely (expansive qiyas is definitionally excluded), while across the wider legal landscape sibling schools persist. Resistance is 0.60: sustained polemic from sibling schools, Sufi pushback, reformist critique, and recurring internal ijtihad revivals from Ibn Taymiyya to contemporary fiqh academies. All three series run on one shared seven-point grid so no metric row is silently substituted.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat compute differently: from the mujtahid seat the regime is fidelity it stewards and the extraction it collects is deferred authority; from the rationalist and customary seats the same structure operates as enforced foreclosure of their development. Two same-level contrasts sharpen the gap: rationalist_jurists and modern_reformers share the moderate power atom but differ in exit (mobile skill-transfer versus constrained institutional embedding), and rival_madhhab_establishments share the institutional power atom with the mujtahids but relate to the constraint as excluded competitor rather than administrator. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real collection: mujtahids collect authority rents, hadith scholars collect the centrality of their discipline, lay believers collect provenance assurance. Victim declarations map to real bearing: rationalist jurists lose tool legitimacy, customary practitioners lose practice legality, reformers lose instrument access. Customary practitioners sit nearest the full-target end (powerless, trapped, generational exposure); rationalist jurists are damped slightly by mobile exit; lay believers are pulled toward symmetric by their dual beneficiary/payer position. No directionality overrides are authored: the derivation from declared roles plus exit options captures the seats, and the power_atom-keyed override granularity would misfire across seats sharing atoms (organized: hadith scholars versus lay believers; moderate: rationalist jurists versus reformers), so the coarse instrument is left unused.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents two opposite mislabels. Reading the regime as pure snare would erase its genuine coordination function — verifiable provenance, bounded innovation, trans-regional uniformity — which the mihna-era origin and the real labor of authentication contradict. Reading it as pure rope would erase the identifiable victim classes and the enforced asymmetry of the authority transfer. On mandatrophy proper: the founding problem (distinguishing revelation from invention) is live, so no dead-mandate declaration is authored; the drift to watch is theater growth and widening breadth of the blocking doctrine, not mandate death. Identity-lock dynamics bind two seats: for the mujtahids the lock is institutional-professional (their authority IS the method, so relaxing it dissolves their standing), and for lay believers it is relational-ideological (congregation and self-conception are constituted through textualist assurance); if either frame broke, those seats' computed extraction and suppression would shift sharply. Suppression mixes structural enforcement (policing institutions, censure) with internalized components (believers who carry the innovation-frame with them after formal exit); the omega variable exit_realism_for_adherents carries that ambiguity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of the usul_al_fiqh_method kernel — the hanbali_reading. What structural features change under the sibling readings (hanafi, maliki, shafii), and where exactly is the disagreement located?',
    'Comparative classification of the sibling stories: each sibling instantiates a different source hierarchy with its own epsilon, beneficiaries, and victims; the disagreement locates in the evidentiary weight assigned to non-textual sources (analogical reason, customary practice, public interest) relative to revelation.',
    'If the sibling readings were merged into one constraint, epsilon would average across incompatible source hierarchies and per-seat classifications would blur; keeping them separate preserves the indexical contrast the kernel contest exists to measure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one reading of a four-reading kernel; disagreement located in non-textual source weight.').

omega_variable(
    authentication_boundary_judgment,
    'Where does ''authenticated hadith'' end and juristic judgment begin, given that authentication itself is an exercise of reasoned judgment?',
    'Track variance in grading decisions across Hanbali authorities for the same reports; measure how often rulings flip when grading standards shift.',
    'If the textual/rational boundary is drawn by rational procedures, part of the claimed extraction-from-rationalism is self-referential and the reading''s self-assessed extraction is overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authentication_boundary_judgment, conceptual, 'Whether the textual/rational boundary is stable or drawn by the very reasoning it excludes.').

omega_variable(
    weak_hadith_net_effect,
    'Does preferring weak hadith over qiyas reduce extraction by avoiding rationalist overreach, or increase it by grounding binding rulings in unreliable reports?',
    'Longitudinal tally of rulings grounded in weak hadith that were later reversed or regraded, compared against reversal rates for rulings derived by explicit qiyas in sibling-school practice.',
    'Higher reversal rates under weak-hadith grounding would raise effective extraction experienced at payer seats (instability imposed on the ruled) and push computed types toward snare-flavored operation at those seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(weak_hadith_net_effect, empirical, 'Net extraction effect of the weak-hadith-over-qiyas preference.').

omega_variable(
    sadd_proportionality,
    'Is sadd al-dhara''i applied proportionately (blocking probable routes to established harm) or expansively (blocking anything carrying disapproval risk)?',
    'Code historical and contemporary fatwa corpora for the breadth of acts blocked under sadd reasoning versus the harms cited in justification.',
    'Expansive application raises suppression and extraction at payer seats and dates a tangled_rope-to-snare drift; proportional application supports the coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sadd_proportionality, empirical, 'Proportionality of the blocking-of-means doctrine in application.').

omega_variable(
    exit_realism_for_adherents,
    'Is cross-madhhab exit a live alternative for embedded adherents, or is it practically closed by community, education, and identity costs?',
    'Post-exit trajectory study of jurists and laypeople who switch methodological allegiance: if condemnation-sensitivity persists after formal exit, part of the measured suppression is internalized rather than structural.',
    'If exit is effectively closed, effective suppression at adherent seats is higher than the structural measure suggests and those seats compute nearer full-target directionality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_realism_for_adherents, empirical, 'Structural versus internalized suppression at the adherent seats.').

omega_variable(
    cs_framing_underdetermination,
    'Is the fixed_text-plus-lineage framing the only defensible commitment-system reading of this constraint, or does a distributed framing (four co-legitimate madhhabs with no single adjudicator) fit the same kernel?',
    'Test whether any institution successfully adjudicates across readings: if madhhab boundaries remain hard and no body resolves cross-school methodological disputes, the distributed framing gains force.',
    'Under a distributed framing, authority_grounding becomes distributed, interpretation_layer_present becomes invalid, and the authored practice_drift reads as healthy plural variation rather than unauthorized departure from the reference frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Alternative commitment-system framings of the same kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__hanbali_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_tr_t0, usul_al_fiqh_method__hanbali_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(usul_tr_t10, usul_al_fiqh_method__hanbali_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(usul_tr_t20, usul_al_fiqh_method__hanbali_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(usul_tr_t30, usul_al_fiqh_method__hanbali_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement(usul_tr_t40, usul_al_fiqh_method__hanbali_reading, theater_ratio, 40, 0.25).
narrative_ontology:measurement(usul_tr_t50, usul_al_fiqh_method__hanbali_reading, theater_ratio, 50, 0.27).
narrative_ontology:measurement(usul_tr_t60, usul_al_fiqh_method__hanbali_reading, theater_ratio, 60, 0.28).

% Extraction over time
narrative_ontology:measurement(usul_be_t0, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(usul_be_t10, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(usul_be_t20, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 20, 0.43).
narrative_ontology:measurement(usul_be_t30, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 30, 0.46).
narrative_ontology:measurement(usul_be_t40, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 40, 0.48).
narrative_ontology:measurement(usul_be_t50, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 50, 0.5).
narrative_ontology:measurement(usul_be_t60, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 60, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(usul_su_t0, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 0, 0.44).
narrative_ontology:measurement(usul_su_t10, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 10, 0.47).
narrative_ontology:measurement(usul_su_t20, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(usul_su_t30, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement(usul_su_t40, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 40, 0.54).
narrative_ontology:measurement(usul_su_t50, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 50, 0.56).
narrative_ontology:measurement(usul_su_t60, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 60, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__hanbali_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanbali_reading, usul_al_fiqh_method__hanafi_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanbali_reading, usul_al_fiqh_method__maliki_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanbali_reading, usul_al_fiqh_method__shafii_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'usul al-fiqh' conflates four structurally distinct source-hierarchy regimes; per the epsilon-invariance principle each reading is a separate story with its own epsilon, beneficiaries, and victims, linked as a constraint family. Upstream/downstream: the shafii_reading's systematization of usul as a meta-discipline supplies the shared grammar within which the hanbali_reading articulates itself (sibling-to-this influence); the hanbali_reading's textual-maximalist pole exerts structural pressure back on the siblings' operating environments as its institutional carrier diffuses (declared here as influences toward shafii, coexistence toward hanafi and maliki). Epsilon contrast across the family: each reading authors epsilon for its own regime by its own lights — the hanbali regime lands mid-range because the reading itself concedes foreclosed development; sibling stories locate their own values independently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
